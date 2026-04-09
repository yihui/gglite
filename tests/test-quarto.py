"""Browser test for Quarto output.

Loads the rendered HTML from tests/test-gglite.html in a headless Chromium
browser and asserts that the expected number of G2 canvas elements are
present and no JavaScript errors occur.
"""
import asyncio, os, sys
from playwright.async_api import async_playwright


async def test(html_path: str, min_canvases: int = 3) -> None:
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        page = await browser.new_page()

        g2_errors = []
        page.on('pageerror', lambda e: g2_errors.append(str(e)))

        await page.goto(f'file://{os.path.abspath(html_path)}')
        await page.wait_for_selector('canvas', timeout=10000)

        canvases = await page.query_selector_all('canvas')
        print(f'Quarto: {len(canvases)} canvas elements')

        errs = [e for e in g2_errors if 'G2' in e or 'chart' in e.lower()]
        if errs:
            raise AssertionError(f'G2 errors in Quarto output: {errs}')
        assert len(canvases) >= min_canvases, (
            f'Expected >= {min_canvases} canvases, got {len(canvases)}'
        )
        print('Quarto test PASSED')

        screenshot = os.environ.get('SCREENSHOT_PATH', '/tmp/quarto-test.png')
        await page.screenshot(path=screenshot, full_page=True)
        await browser.close()


if __name__ == '__main__':
    html = sys.argv[1] if len(sys.argv) > 1 else 'tests/test-gglite.html'
    asyncio.run(test(html))
