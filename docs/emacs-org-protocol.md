# Emacs Org Protocol

Home Manager starts an Emacs daemon with the graphical session and registers
`emacsclient.desktop` as the handler for `org-protocol://` URLs. Emacs loads
`org-roam-protocol`, which adds the `roam-ref` handler used to create or visit
an Org Roam node for a web page. The derived Org Roam database stays in the
local XDG cache rather than the synchronizable `~/org` tree.

The browser bookmark is intentionally manual state because Firefox owns and
syncs its bookmarks. Add a bookmark in Firefox and use the following single line
as its URL:

<!-- prettier-ignore -->
```javascript
javascript:(()=>{const canonical=document.querySelector('link[rel="canonical"]')?.href;const ref=new URL(canonical||location.href);for(const key of [...ref.searchParams.keys()]){if(/^utm_/i.test(key)||/^(fbclid|gclid|dclid|mc_cid|mc_eid)$/i.test(key))ref.searchParams.delete(key)}ref.hash='';location.href='org-protocol://roam-ref?'+new URLSearchParams({template:'r',ref:ref.href,title:document.title.replace(/\s+/g,' ').trim(),body:String(getSelection()).trim()});void 0})()
```

## How the bookmarklet works

The same code expanded for readability is:

```javascript
(() => {
  // Prefer the publisher's stable page identity over the current navigated URL.
  const canonical = document.querySelector('link[rel="canonical"]')?.href;

  // Fall back to the current URL and parse it before modifying its components.
  const ref = new URL(canonical || location.href);

  // Copy the keys because deleting entries while iterating a live URLSearchParams
  // iterator can otherwise skip subsequent parameters.
  for (const key of [...ref.searchParams.keys()]) {
    // Remove common campaign and click identifiers without discarding meaningful
    // parameters such as article, issue, or search IDs.
    if (
      /^utm_/i.test(key) ||
      /^(fbclid|gclid|dclid|mc_cid|mc_eid)$/i.test(key)
    ) {
      ref.searchParams.delete(key);
    }
  }

  // Treat section links as references to the page rather than separate nodes.
  ref.hash = "";

  // Navigating to the custom scheme delegates the encoded values to the desktop
  // handler, which forwards them to org-roam-protocol through emacsclient.
  location.href =
    "org-protocol://roam-ref?" +
    new URLSearchParams({
      // Select the matching key in org-roam-capture-ref-templates.
      template: "r",

      // Store the normalized page identity in the node's ROAM_REFS property.
      ref: ref.href,

      // Collapse tabs, newlines, and repeated spaces in browser-generated titles.
      title: document.title.replace(/\s+/g, " ").trim(),

      // Seed the capture with selected page text, excluding surrounding whitespace.
      body: String(getSelection()).trim(),
    });

  // Ensure the bookmarklet itself does not return a renderable string.
  void 0;
})();
```

1. `link[rel="canonical"]` uses the publisher's canonical URL when the page
   provides one. Otherwise, the current browser URL is used.
2. `new URL(...)` parses the reference so its query parameters and fragment can
   be normalized safely.
3. Common advertising and campaign identifiers are removed. Meaningful query
   parameters, such as an article ID, remain intact.
4. The fragment is removed so links to different sections of one page resolve to
   the same Org Roam reference.
5. `URLSearchParams` encodes the capture parameters:
   - `template=r` selects the `r` entry in `org-roam-capture-ref-templates`.
   - `ref` becomes the node's `ROAM_REFS` value.
   - `title` supplies the node title after whitespace normalization.
   - `body` supplies the selected page text as the capture's initial content.
     The reference template expands it with `%i` inside an Org quote block and
     places `%?` afterward for additional notes.
6. Assigning the resulting URL to `location.href` asks Firefox to dispatch it
   through the desktop's registered `org-protocol` handler.
7. `void 0` prevents a returned string from replacing the current page in
   browsers that render bookmarklet return values.

Canonical URLs are occasionally incorrect, and removing fragments deliberately
collapses section-level links into one page-level reference. Use the current
page URL and retain `ref.hash` if either behavior is undesirable. Avoid very
large selections because the selected text is transported inside the URL.

## Activation and validation

After changing this configuration, activate the appropriate Home Manager
profile. Then verify the service and handler:

```bash
systemctl --user status emacs.service
xdg-mime query default x-scheme-handler/org-protocol
emacsclient --eval "(featurep 'org-roam-protocol)"
```

The expected handler is `emacsclient.desktop`, and the feature check should
return `t`. Clicking the bookmark should open the `r` Org Roam reference capture
template. Finalizing or aborting the capture closes its temporary frame.

Synchronize the authoritative `.org` files rather than `org-roam.db`. Org Roam
rebuilds its per-machine database at
`${XDG_CACHE_HOME:-$HOME/.cache}/emacs/org-roam.db`.

The shell aliases provide related entry points:

- `et` attaches an Emacs client to the current terminal.
- `ef` opens a detached graphical Emacs frame.
- `ec` opens a detached Org capture frame without a browser reference.

See the [Org Roam protocol manual](https://www.orgroam.com/manual) and the
[Org protocol manual](https://orgmode.org/manual/Protocols.html) for the
protocol and capture semantics.
