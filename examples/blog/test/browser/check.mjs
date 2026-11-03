import puppeteer from 'puppeteer-core';

const BASE = process.env.BASE || 'http://localhost:8080';
const CHROME = process.env.CHROME;
if (!CHROME) {
  console.error('Set CHROME to a Chrome or Chromium binary, e.g. CHROME=/usr/bin/google-chrome');
  process.exit(2);
}

const browser = await puppeteer.launch({ executablePath: CHROME, headless: true });
const problems = [];

function watch(page, name) {
  page.on('console', m => {
    if (m.type() === 'error' || m.type() === 'warn') problems.push(`${name} console ${m.type()}: ${m.text()}`);
  });
  page.on('pageerror', e => problems.push(`${name} page error: ${e.message}`));
  page.on('response', r => {
    if (r.status() >= 400) problems.push(`${name} http ${r.status()} ${r.url()}`);
  });
  return page;
}

const connected = page => page.waitForSelector('html.az-connected', { timeout: 5000 });
const textIs = (page, selector, text) =>
  page.waitForFunction(
    (s, t) => document.querySelector(s)?.textContent.trim() === t,
    { timeout: 5000 }, selector, text);

async function step(name, fn) {
  await fn();
  console.log(`ok   ${name}`);
}

try {
  const author = watch(await browser.newPage(), 'author');
  const readerContext = await browser.createBrowserContext();
  const reader = watch(await readerContext.newPage(), 'reader');
  const name = `browser${Date.now()}`;
  const title = `Checked in a browser ${name}`;

  await step('counter counts', async () => {
    await author.goto(`${BASE}/live/counter`);
    await connected(author);
    await author.click('#inc');
    await textIs(author, '#count', '1');
    await author.click('#dec');
    await textIs(author, '#count', '0');
  });

  await step('register and log in', async () => {
    await author.goto(`${BASE}/register`);
    await author.type('input[name=email]', `${name}@example.com`);
    await author.type('input[name=username]', name);
    await author.type('input[name=password]', 'correct horse battery');
    await author.type('input[name=password_confirmation]', 'correct horse battery');
    await Promise.all([author.waitForNavigation(), author.click('form button[type=submit]')]);
    if (new URL(author.url()).pathname !== '/posts') throw new Error(`landed on ${author.url()}`);
  });

  await step('write and publish a post', async () => {
    await author.goto(`${BASE}/live/editor`);
    await connected(author);
    await author.type('#post-title', title);
    await author.type('#post-body', 'Written by a headless browser.');
    await author.click('#post-form button[value=publish]');
    await author.waitForFunction(() => location.pathname.startsWith('/live/posts/'));
    await textIs(author, '#post h1', title);
    await textIs(author, '#post .flash', 'Your post is live');
  });

  await step('a comment reaches another reader', async () => {
    await reader.goto(author.url());
    await connected(reader);
    await author.type('#comment-body', 'First!');
    await author.click('#comment-form button[type=submit]');
    await textIs(reader, '#comment-list li:last-child .comment-body', 'First!');
    await author.waitForFunction(() => document.querySelector('#comment-body').value === '');
  });

  if (problems.length) throw new Error(problems.join('\n'));
  console.log('browser check passed');
} finally {
  await browser.close();
}
