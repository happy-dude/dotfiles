import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { runInNewContext } from "node:vm";

const documentPath =
  process.env.DOTFILES_ORG_PROTOCOL_DOC ??
  new URL("../docs/emacs-org-protocol.md", import.meta.url);
const forms = Array.from(
  readFileSync(documentPath, "utf8").matchAll(
    /```javascript\n([\s\S]*?)\n```/g,
  ),
).slice(0, 2);
if (forms.length !== 2) {
  throw new Error("Both published bookmarklet forms are required");
}

function capture(code, { title, href, selection = "", activeElement = null }) {
  const notices = [];
  const navigations = [];
  const location = {
    get href() {
      return href;
    },
    set href(value) {
      navigations.push(value);
    },
  };
  runInNewContext(
    code.replace(/^javascript:/, ""),
    {
      URL,
      URLSearchParams,
      location,
      document: { title, activeElement, querySelector: () => null },
      window: { getSelection: () => ({ toString: () => selection }) },
      alert: (message) => notices.push(message),
    },
    { timeout: 1000 },
  );
  return { notices, navigations };
}

for (const [index, [, code]] of forms.entries()) {
  const form = index === 0 ? "bookmark URL" : "expanded code";

  test(`${form}: preserve page identity and focused text without credentials or tracking`, () => {
    const { notices, navigations } = capture(code, {
      title: "  A\n  title ",
      href: "https://name:password@example.org/article?id=42&utm_source=mail#section",
      selection: "not the focused selection",
      activeElement: {
        tagName: "TEXTAREA",
        value: "skip 選んだ文 end",
        selectionStart: 5,
        selectionEnd: 9,
      },
    });
    assert.deepEqual(notices, []);
    assert.equal(navigations.length, 1);
    const result = new URL(navigations[0]);
    assert.equal(result.protocol, "org-protocol:");
    assert.equal(result.hostname, "roam-ref");
    assert.deepEqual(Object.fromEntries(result.searchParams), {
      template: "r",
      ref: "https://example.org/article?id=42",
      title: "A title",
      body: "選んだ文",
    });
  });

  test(`${form}: truncate selected Unicode text at the encoded URI boundary`, () => {
    const { notices, navigations } = capture(code, {
      title: "Title",
      href: "https://example.org/article",
      selection: "😀".repeat(1000),
    });
    assert.deepEqual(notices, []);
    assert.equal(navigations.length, 1);
    assert.ok(navigations[0].length <= 8000);
    const result = new URL(navigations[0]);
    const body = result.searchParams.get("body");
    assert.match(body, /^(?:😀)+$/u);
    result.searchParams.set("body", body + "😀");
    assert.ok(result.href.length > 8000);
  });

  test(`${form}: refuse metadata that cannot fit without changing page identity`, () => {
    for (const metadata of [
      { title: "測".repeat(1000), href: "https://example.org/article" },
      { title: "Title", href: "https://example.org/" + "x".repeat(8000) },
    ]) {
      const { notices, navigations } = capture(code, metadata);
      assert.deepEqual(navigations, []);
      assert.equal(notices.length, 1);
    }
  });
}
