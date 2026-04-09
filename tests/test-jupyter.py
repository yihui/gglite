"""Browser test for Jupyter notebook output.

Reads the executed notebook, extracts HTML cell outputs, and loads each one
inside an iframe (matching JupyterLab's sandboxed rendering model) to verify
that G2 charts render correctly with no JavaScript errors.
"""
import asyncio, json, os, sys, tempfile
from playwright.async_api import async_playwright


async def test(nb_path: str, min_charts: int = 3) -> None:
    with open(nb_path) as f:
        nb = json.load(f)

    # Collect text/html outputs from all code cells
    chart_htmls = []
    for cell in nb['cells']:
        for out in cell.get('outputs', []):
            if 'text/html' in out.get('data', {}):
                chart_htmls.append(''.join(out['data']['text/html']))

    print(f'Found {len(chart_htmls)} chart outputs in notebook')
    assert len(chart_htmls) >= min_charts, (
        f'Expected >= {min_charts} chart outputs, got {len(chart_htmls)}'
    )

    # Write each chart to a separate file for iframe loading
    tmp_dir = tempfile.mkdtemp()
    iframe_tags = []
    for i, html in enumerate(chart_htmls):
        path = os.path.join(tmp_dir, f'chart{i}.html')
        with open(path, 'w') as f:
            f.write(html)
        iframe_tags.append(
            f'<iframe src="file://{path}" width="800" height="520" '
            f'frameborder="0" id="frame{i}"></iframe>'
        )

    wrapper = os.path.join(tmp_dir, 'wrapper.html')
    with open(wrapper, 'w') as f:
        f.write('<!DOCTYPE html><html><body>\n')
        f.write('\n'.join(iframe_tags))
        f.write('\n</body></html>')

    async with async_playwright() as p:
        browser = await p.chromium.launch()
        page = await browser.new_page()

        errors = []
        page.on('pageerror', lambda e: errors.append(str(e)))

        await page.goto(f'file://{wrapper}')

        # Poll for canvases in iframes (CDN scripts load asynchronously)
        iframe_canvases = 0
        for _ in range(12):
            iframe_canvases = 0
            for iframe_el in await page.query_selector_all('iframe'):
                frame = await iframe_el.content_frame()
                if frame:
                    cs = await frame.query_selector_all('canvas')
                    iframe_canvases += len(cs)
            if iframe_canvases >= min_charts:
                break
            await page.wait_for_timeout(1000)

        print(f'Jupyter: {iframe_canvases} canvas elements across iframes')

        errs = [e for e in errors if 'G2' in e]
        if errs:
            raise AssertionError(f'G2 errors in Jupyter output: {errs}')
        assert iframe_canvases >= min_charts, (
            f'Expected >= {min_charts} canvases, got {iframe_canvases}'
        )
        print('Jupyter test PASSED')

        screenshot = os.environ.get('SCREENSHOT_PATH', '/tmp/jupyter-test.png')
        await page.screenshot(path=screenshot, full_page=True)
        await browser.close()


if __name__ == '__main__':
    nb = sys.argv[1] if len(sys.argv) > 1 else '/tmp/test-gglite-executed.ipynb'
    asyncio.run(test(nb))
