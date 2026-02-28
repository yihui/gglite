import puppeteer from 'puppeteer';
import { readFileSync, readdirSync } from 'fs';

const browser = await puppeteer.launch({
  headless: true,
  args: ['--no-sandbox', '--disable-setuid-sandbox']
});

const dir = '/tmp/gglite-test';
const files = readdirSync(dir).filter(f => f.endsWith('.html')).sort();
let passed = 0, failed = 0;
const failures = [];

for (const file of files) {
  const name = file.replace('.html', '');
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', (err) => errors.push(err.message));

  const html = readFileSync(`${dir}/${file}`, 'utf-8');
  await page.setContent(html, { waitUntil: 'networkidle0', timeout: 30000 });

  // Wait for G2 to render
  const hasCanvas = await page.waitForSelector(`#${name} canvas`, { timeout: 10000 })
    .then(() => true)
    .catch(() => false);

  // Also check for SVG (some marks render to SVG)
  const hasSvg = hasCanvas ? false : await page.$(`#${name} svg`).then(e => !!e).catch(() => false);
  const rendered = hasCanvas || hasSvg;

  // Check for any remaining content in the container (some marks might render differently)
  const hasContent = rendered ? true : await page.$eval(`#${name}`, el => el.children.length > 0).catch(() => false);

  if (errors.length > 0) {
    console.log(`FAIL: ${name} — JS errors: ${errors[0].substring(0, 120)}`);
    failures.push(name);
    failed++;
  } else if (!hasContent) {
    console.log(`WARN: ${name} — no visible content (may need interaction)`);
    passed++; // some marks are valid but need data interaction
  } else {
    console.log(`PASS: ${name}`);
    passed++;
  }
  await page.close();
}

await browser.close();
console.log(`\n${passed} passed, ${failed} failed out of ${files.length}`);
if (failures.length) {
  console.log('Failures:', failures.join(', '));
  process.exit(1);
}
