(function () {
  var cache = new Map();
  var popup = null;
  var timer = null;
  var activeLink = null;

  function createPopup() {
    var el = document.createElement('div');
    el.className = 'link-preview';
    el.setAttribute('role', 'tooltip');
    document.body.appendChild(el);
    return el;
  }

  function showPreview(link) {
    var href = link.getAttribute('href');
    // Link previews only apply to notes (other sections don't have prose content to preview)
    if (!href || !href.startsWith('/notes/')) return;

    activeLink = link;

    timer = setTimeout(function () {
      if (activeLink !== link) return;

      if (cache.has(href)) {
        displayPopup(link, cache.get(href));
      } else {
        fetch(href)
          .then(function (r) { return r.text(); })
          .then(function (html) {
            var doc = new DOMParser().parseFromString(html, 'text/html');
            var body = doc.querySelector('.note-body');
            var p = body ? body.querySelector('p') : null;
            var text = p ? p.textContent.trim() : '';
            if (text.length > 200) text = text.substring(0, 200) + '\u2026'; // Roughly one paragraph for the popup
            cache.set(href, text);
            if (activeLink === link) {
              displayPopup(link, text);
            }
          })
          .catch(function () { cache.set(href, ''); }); // Cache empty string on failure to prevent repeated fetch attempts
      }
    }, 150); // Debounce: avoid fetching on brief hover-throughs
  }

  function displayPopup(link, text) {
    if (!text) return;
    if (!popup) popup = createPopup();
    popup.textContent = text;

    var rect = link.getBoundingClientRect();
    popup.style.left = rect.left + window.scrollX + 'px';
    popup.style.top = (rect.bottom + window.scrollY + 6) + 'px'; // 6px visual gap between link and popup
    popup.classList.add('visible');
  }

  function hidePreview() {
    activeLink = null;
    clearTimeout(timer);
    if (popup) popup.classList.remove('visible');
  }

  // Capture phase required: pointerenter/pointerleave don't bubble, so delegation only works via capture
  document.addEventListener('pointerenter', function (e) {
    var link = e.target.closest('a[href^="/notes/"]');
    if (link) showPreview(link);
  }, true);

  document.addEventListener('pointerleave', function (e) {
    var link = e.target.closest('a[href^="/notes/"]');
    if (link && (!e.relatedTarget || !link.contains(e.relatedTarget))) {
      hidePreview();
    }
  }, true);

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hidePreview();
  });
})();
