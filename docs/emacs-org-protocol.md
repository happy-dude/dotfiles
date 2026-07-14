# Emacs Org Protocol

Home Manager starts an Emacs daemon with the graphical session and registers a
dedicated `emacs-org-protocol.desktop` as the handler for `org-protocol://`
URLs. Its `%u` field passes the protocol URL to a non-blocking graphical
`emacsclient` after an option terminator; the ordinary Emacs desktop entry
remains responsible for opening files. Emacs loads `org-roam-protocol`, which
adds the `roam-ref` handler used to create or visit an Org Roam node for a web
page. The handler marks that client frame as the temporary capture frame instead
of splitting the capture into another window. Its display override takes
precedence over Org's explicit split-window capture action. The derived Org Roam
database and undo-tree history stay in the local XDG cache rather than the
synchronizable `~/org` tree.

The browser bookmark is intentionally manual state because Firefox owns and
syncs its bookmarks. Add a bookmark in Firefox and use the following single line
as its URL. This is the canonical bookmarklet; copy the line exactly because
missing spaces, periods, or regular-expression delimiters make it invalid:

<!-- prettier-ignore -->
```javascript
javascript:(()=>{const canonical=document.querySelector('link[rel="canonical"]')?.href;const ref=new URL(canonical||location.href,document.baseURI);for(const key of [...ref.searchParams.keys()]){if(/^utm_/i.test(key)||/^(fbclid|gclid|dclid|mc_cid|mc_eid)$/i.test(key))ref.searchParams.delete(key)}ref.hash='';location.href='org-protocol://roam-ref?'+new URLSearchParams({template:'r',ref:ref.href,title:document.title.replace(/\s+/g,' ').trim(),body:String(getSelection()).trim()});void 0})()
```

Optionally assign the Firefox bookmark the keyword `org`. Typing `org` in the
address bar and pressing Enter then invokes the bookmarklet on the current page.
The JavaScript does not contain the keyword, and its URL deliberately has no
`%s` placeholder because it does not accept keyword-search text.

## How the bookmarklet works

The same code expanded for readability is:

```javascript
(() => {
  // Prefer the publisher's stable page identity over the current navigated URL.
  const canonical = document.querySelector('link[rel="canonical"]')?.href;

  // Fall back to the current URL and resolve relative canonical references
  // against the document base before modifying the URL components.
  const ref = new URL(canonical || location.href, document.baseURI);

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
2. `new URL(..., document.baseURI)` resolves absolute and relative canonical
   references before their query parameters and fragments are normalized.
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
   browsers that render bookmarklet return values. It does not cancel the
   explicit assignment to `location.href`.

Canonical URLs are occasionally incorrect, and removing fragments deliberately
collapses section-level links into one page-level reference. Use the current
page URL and retain `ref.hash` if either behavior is undesirable. Avoid very
large selections because the selected text is transported inside the URL.

Firefox normally hands the external scheme to the desktop handler without
replacing the source page. Opening the protocol URL through `window.open()` may
instead trigger popup blocking or leave a blank window, so the canonical
bookmarklet deliberately retains `location.href`.

### Comparison with the minimal bookmarklet

A common minimal bookmarklet constructs the protocol URL directly:

<!-- prettier-ignore -->
```javascript
javascript:location.href =
    'org-protocol://roam-ref?template=r&ref='
    + encodeURIComponent(location.href)
    + '&title='
    + encodeURIComponent(document.title)
    + '&body='
    + encodeURIComponent(window.getSelection())
```

That form correctly percent-encodes the current URL, page title, and selection.
The canonical dotfiles bookmarklet additionally:

- prefers the publisher's canonical URL and resolves relative canonical
  references against `document.baseURI`;
- removes common campaign and click identifiers while retaining meaningful query
  parameters;
- removes the fragment so sections of one page share an Org Roam reference;
- normalizes title whitespace and trims the selected text;
- uses `URLSearchParams` to encode the complete parameter map rather than
  manually interleaving field names and encoded values; and
- returns `undefined` explicitly so the bookmarklet has no renderable result.

Both forms assign the custom-scheme URL to `location.href`; neither prevents
that explicit navigation. The desktop handoff and popup tradeoffs therefore
apply equally to both. The configured `r` reference template consumes `body`
through `%i`, quotes the selection, and leaves `%?` at the note insertion point.

## Activation and validation

After changing this configuration, activate the appropriate Home Manager
profile. Then verify the service and handler:

```bash
systemctl --user status emacs.service
xdg-mime query default x-scheme-handler/org-protocol
emacsclient --eval "(featurep 'org-roam-protocol)"
```

The expected handler is `emacs-org-protocol.desktop`, and the feature check
should return `t`. Clicking the bookmark should open the `r` Org Roam reference
capture template. Finalizing or aborting the capture closes its temporary frame.
A reference that already exists opens its node in the dedicated frame instead of
starting another capture.

### Flatpak Firefox

Flatpak Firefox dispatches external URLs through the desktop portal. A host-side
`xdg-open org-protocol://...` test validates the desktop entry and Emacs, but it
does not exercise the Flatpak-to-portal boundary. Test the actual bookmarklet in
Firefox as a separate step.

Use this temporary bookmarklet to confirm that Firefox can read the selection
before diagnosing external URL dispatch:

<!-- prettier-ignore -->
```javascript
javascript:(()=>{alert(JSON.stringify(String(window.getSelection())));void 0})()
```

If the selection test works and Emacs opens without receiving a capture, verify
the handler and refresh the desktop application cache:

```bash
flatpak ps | rg -i firefox
xdg-mime query default x-scheme-handler/org-protocol
gio mime x-scheme-handler/org-protocol
update-desktop-database ~/.local/share/applications
kbuildsycoca6 --noincremental
systemctl --user list-units --type=service 'xdg-desktop-portal*'
```

Portal backend unit names vary by desktop and distribution. Restart only the
active portal units shown by the final command, fully exit Firefox, and relaunch
it before retesting. KDE may otherwise retain the previous desktop entry and
discard the protocol URL even though `xdg-mime` reports the new default.

Synchronize the authoritative `.org` files rather than `org-roam.db`. Org Roam
rebuilds its per-machine database at
`${XDG_CACHE_HOME:-$HOME/.cache}/emacs/org-roam.db`. Undo-tree history is
similarly stored below `${XDG_CACHE_HOME:-$HOME/.cache}/emacs/undo-tree/`.

The shell aliases provide related entry points:

- `et` attaches an Emacs client to the current terminal.
- `ef` opens a detached graphical Emacs frame.
- `ec` opens a detached Org capture frame without a browser reference.

See the [Org Roam protocol manual](https://www.orgroam.com/manual) and the
[Org protocol manual](https://orgmode.org/manual/Protocols.html) for the
protocol and capture semantics.
