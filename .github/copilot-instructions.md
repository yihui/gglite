# gglite - Repository Instructions for Copilot

## Repository Overview

This is an R package that provides a lightweight interface to the AntV G2
JavaScript visualization library with a ggplot2-style API. It supports rendering
in R Markdown, litedown, Shiny, and standalone HTML previews.

**Project Type**: R package **Languages**: R, JavaScript (via CDN) **Size**:
Small repository (\~15 R source files)

## Build and Test Instructions

### Prerequisites

R has been automatically installed via `.github/copilot-setup-steps.yml` when
working with GitHub Copilot.

### Bootstrap and Build Sequence

1.  **Build the R package**:

    ``` bash
    R CMD build .
    ```

2.  **Install the package**:

    ``` bash
    R CMD INSTALL *_*.tar.gz
    ```

### Testing Conventions

-   Tests are in `tests/testit/test-*.R`
-   Use `testit` package for assertions
-   Always wrap test conditions in `{}`: `assert('message', {})`
-   Use `has_error()` instead of `tryCatch()` for error testing
-   Load the package with `library(gglite)` before testing
-   Use `%==%` (from testit) instead of `==` to test for strict equality
-   Never use `:::` to access internal functions in tests; testit exposes
    internal functions automatically, so call them directly

### Rendering Examples to HTML

All `examples/*.Rmd` files are rendered using **litedown**, not rmarkdown. Never
use `rmarkdown::render()` — it will produce incorrect output or fail.

To render an Rmd file to HTML (e.g., for inspection or headless browser
testing):

``` bash
Rscript -e 'litedown::fuse("path/to/foo.Rmd")'
# output: path/to/foo.html
```

This will render a self-contained HTML file (since we have enabled the
`embed_resources` option for litedown in `copilot-setup-steps.yml`). All
external JS/CSS resources are embedded, so they won't be blocked when testing
via headless browsers.

Similarly, for an R script that generates a g2 plot, you can render it to HTML
via:

``` bash
Rscript -e 'litedown::fuse("path/to/foo.R')'
# output: path/to/foo.html
```

When you do headless browser testing for an example, you must always write the
example code to an .R file and render it this way.

The GitHub Pages site is built by the `yihui/litedown/site` action, which calls
`litedown::fuse()` for every Rmd in the repo.

### Testing Plots in Headless Browsers

Since gglite generates HTML/JavaScript visualizations, **plots must be tested in
headless browsers** to make sure they can be rendered correctly and produce no
errors in the browser console. The workflow is:

1.  **Render to a full HTML page** — both `.Rmd` and `.R` files can be rendered
    to `.html` via `litedown::fuse()`. The output is self-contained (all JS/CSS
    embedded) because `embed_resources` is enabled in `copilot-setup-steps.yml`.

2.  **Open with `google-chrome` under Xvfb** and enable remote debugging. Use
    `google-chrome`, **not** `chromium` — `chromium` crashes in this
    environment. Omit `--no-zygote` and `--single-process`; both cause crashes.
    Use `file://` absolute paths — **Chrome cannot reach `127.0.0.1`** in this
    sandbox so `http://` URLs always fail with a connection error.

    ``` bash
    Xvfb :99 -screen 0 1280x1024x24 2>/dev/null &
    DISPLAY=:99 google-chrome --no-sandbox --disable-gpu \
      --disable-dev-shm-usage \
      --remote-debugging-port=9222 \
      "about:blank" 2>/dev/null &
    sleep 10   # wait for Chrome to start (needs ~10s in this environment)
    ```

    > **Important:** The `playwright-browser_*` tools are sandboxed from the
    > loopback interface. Do **not** use them for local pages. Use the
    > `google-chrome` + CDP approach above instead.

3.  **Query the live DOM via CDP** (Chrome DevTools Protocol) using Python's
    `websockets` package (`pip install websockets`). Always pass
    `max_size=10*1024*1024` to handle large embedded pages:

    ``` python
    import asyncio, json, base64
    import websockets

    WS_URL = (lambda: __import__('json').loads(
        __import__('urllib.request', fromlist=['urlopen'])
        .urlopen('http://127.0.0.1:9222/json').read()
    )[0]['webSocketDebuggerUrl'])()

    rid = [0]

    async def send(ws, method, params=None):
        rid[0] += 1; r = rid[0]
        await ws.send(json.dumps({"id": r, "method": method, "params": params or {}}))
        while True:
            msg = json.loads(await asyncio.wait_for(ws.recv(), timeout=15))
            if msg.get("id") == r: return msg.get("result", {})

    async def js(ws, expr, await_promise=False):
        return await send(ws, "Runtime.evaluate", {
            "expression": expr, "awaitPromise": await_promise, "returnByValue": True
        })

    async def shot(ws, path):
        r = await send(ws, "Page.captureScreenshot", {"format": "png"})
        with open(path, "wb") as f: f.write(base64.b64decode(r.get("data","")))

    async def main():
        async with websockets.connect(WS_URL, max_size=10*1024*1024) as ws:
            # Use file:// URL — http://127.0.0.1 is blocked in this sandbox
            await send(ws, "Page.navigate",
                {"url": "file:///absolute/path/to/foo.html"})
            await asyncio.sleep(8)  # wait for G2 charts to render

            r = await js(ws, "JSON.stringify({G2: typeof G2 !== 'undefined', "
                             "c: document.querySelectorAll('canvas').length})")
            print("Status:", r["result"]["value"])

            # Hover over a canvas to trigger tooltip
            await send(ws, "Input.dispatchMouseEvent",
                {"type": "mouseMoved", "x": 400, "y": 300})
            await asyncio.sleep(0.8)
            await shot(ws, "/tmp/screenshot.png")

    asyncio.run(main())
    ```

4.  Verify:

    -   The chart container element exists in the DOM.
    -   The G2 chart renders without JavaScript errors (check `console.error`).
    -   No warnings or errors appear in the browser console.

### Submitting Plot Changes in PRs

When fixing or changing plot examples, **always submit screenshots** of the
plots as PR comments so reviewers can see the visual results. Take screenshots
in headless browsers (Chromium via CDP) and attach them to the PR.

### G2 Reference

When dealing with issues that you cannot solve easily, you should dig into G2's
source code and documentation, which have been checked out to the directory `G2`
under the root directory for you. Use that as the source of truth.

-   **G2 source repository**: <https://github.com/antvis/G2>
-   **G2 documentation site**: <https://g2.antv.antgroup.com>

If you have consulted the source repository and documentation but still cannot
solve a problem raised in a PR, **file a GitHub issue for that problem to keep
track of it**. The issue should clearly describe what the problem is, and why
you cannot solve it.

## Project Structure

### CI/CD Configuration

**GitHub Actions** (`.github/workflows/`):

-   `R-CMD-check.yaml` - Runs R CMD check on multiple platforms
-   `copilot-setup-steps.yml` - Sets up the environment for Copilot
-   `github-pages.yml` - Builds and deploys the package site via litedown

## Important Conventions

### R Code Style

1.  **Assignment**: Use `=` instead of `<-` for assignment
2.  **Strings**: Use single quotes for strings (e.g., `'text'`)
3.  **Indentation**: Use 2 spaces (not 4 spaces or tabs)
4.  **Compact code**: Avoid `{}` for single-expression if statements; prefer
    compact forms when possible
5.  **Roxygen documentation**: Don't use `@description` or `@details` explicitly
    — just write the description text directly after the title. Don't use
    `@title` either.
6.  **Examples**: Avoid `\dontrun{}` unless absolutely necessary. Prefer
    runnable examples that can be tested automatically.
7.  **Function definitions**: For functions with many arguments, break the line
    right after the opening `(`, indent arguments by 2 spaces, and try to wrap
    them at 80-char width.
8.  **Re-wrap code**: Always re-wrap the code after making changes to maintain
    consistent formatting and line length.
9.  **JavaScript in R**: Use `const` and arrow functions (`=>`) in JS,
    `type="module"` for inline scripts, `defer` for external scripts.
10. **Implicit NULL**: Don't write `if (cond) foo else NULL`; the `else NULL` is
    unnecessary since R's `if` without `else` already returns `NULL`.
11. **Return NULL**: Never write `return(NULL)`; use `return()` instead since R
    functions return `NULL` by default when no value is given.
12. **US spelling**: Use US spelling throughout all documentation, code
    comments, and example text (e.g., "color" not "colour", "center" not
    "centre", "summarize" not "summarise").
13. **DRY (Don't Repeat Yourself)**: Never duplicate code. When the same logic
    appears more than once, factor it into a shared helper function. This
    applies to expressions, patterns, and multi-line blocks alike.
14. **Example plot reuse**: In Rmd and Rd examples, define a base chart object
    (e.g., `p = g2(df, y ~ x) |> mark_line()`) once and reuse it with `p |>` for
    subsequent variations. Group related examples together so readers can
    compare them without scrolling past unrelated chart types. Never interrupt a
    series of bar-chart examples with a scatter-plot example.
15. **Short pipes on one line**: When a pipe chain fits within 80 characters,
    keep it on a single line. Only break after `|>` when the full expression
    would exceed 80 characters. For example, write
    `p |> slider_x() |> slider_y()` rather than splitting across three lines.

### Variables and Formula Interface

gglite does **NOT** use non-standard evaluation (NSE). Variables can be
specified either as character strings (`x = 'mpg'`) or via the formula interface
(`y ~ x`). **Prefer the formula interface** in examples and documentation
because it is more concise and readable:

``` r
# Preferred: formula interface
g2(mtcars, hp ~ mpg)

# Also valid: character strings
g2(mtcars, x = 'mpg', y = 'hp')
```

For single-variable distributions, omit the LHS:

``` r
g2(mtcars, ~ mpg)   # histogram
```

The formula interface also works for other aesthetic channels by passing a
one-sided formula as a named argument:

``` r
g2(iris, Sepal.Length ~ Sepal.Width, color = ~ Species)
g2(mtcars, hp ~ mpg, color = ~ cyl, size = ~ wt)
g2(iris, Sepal.Length ~ Sepal.Width, shape = ~ Species)
```

**Drop explicit marks that can be automatically inferred.** gglite's
`auto_mark()` detects the appropriate mark from the data types. Only specify a
mark explicitly when you need a non-default one:

``` r
# Preferred: auto-inferred scatter plot
g2(mtcars, hp ~ mpg)

# Only do this when you need something non-default
g2(mtcars, hp ~ mpg) |> mark_line()
```

### Testing Conventions

1.  **Every change must have tests**: Every code change must come with
    corresponding tests. If you add or fix a function, add assertions in the
    test file that cover the new or fixed behavior. Tests are the first place to
    catch regressions and errors.

2.  **Use testit properly**: Write all test conditions in `()`, use `%==%` to
    test for `identical()`, and test conditions can return vectors.

    ``` r
    assert("test description", {
      (length(result) %==% 3L)
      (file.exists(result))
    })
    ```

### Check list

Always send a pull request, unless you are told otherwise. For each PR:

1.  **Always re-roxygenize**: Run `roxygen2::roxygenize()` after changing any
    roxygen documentation to update man files
2.  **MANDATORY: R CMD check before EVERY commit**: You MUST run `R CMD check`
    successfully before submitting ANY code changes.
3.  **MANDATORY: Wait for CI to be green**: After pushing code, you MUST wait
    for GitHub Actions CI to complete successfully before claiming the task is
    done. Do not wait more than 3 minutes for any single CI job; if it hasn't
    finished, skip it and continue your work.
4.  **MANDATORY: Merge latest main before pushing**: Before pushing to a branch
    or PR, always pull and merge the latest main branch. If there are merge
    conflicts, resolve them before pushing.
5.  **Bump version in PRs**: Bump the patch version number in DESCRIPTION once
    per PR (on the first commit or when you first make changes), not on every
    commit to the PR
6.  **NEVER BREAK CI**: Breaking CI is completely unacceptable. If CI fails, you
    must immediately fix it.
7.  **Never commit binary files**: Avoid version-controlling binary files,
    especially automatically generated ones.
8.  **Testing**: Use testit assertions with proper error handling
9.  **Update NEWS.md**: When making changes, make sure to update `NEWS.md`
    accordingly to document what changed — **except for v0.1** (do NOT add
    individual change entries for the initial release). The first heading in
    NEWS.md always represents the dev version and must be of the form
    `# PKG x.y` where PKG is the package name and x.y is the next version to be
    released to CRAN (note: x.y, not x.y.0). Usually y is bumped from the
    current minor version, e.g., if the current dev version is 1.8.3, the next
    CRAN release is expected to be 1.9.

## Package API

The main entry point is `g2()` which creates a chart object, then pipe operators
(`|>`) chain mark, scale, coordinate, interaction, theme, and component
functions:

``` r
g2(mtcars, hp ~ mpg) |>
  scale_x(type = 'log') |>
  theme_dark() |>
  title_('Motor Trend Cars')
```
