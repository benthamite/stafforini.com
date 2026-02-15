(function () {
  var cache = new Map();
  var popup = null;
  var timer = null;

  function createPopup() {
    var el = document.createElement('div');
    el.className = 'link-preview';
    el.setAttribute('role', 'tooltip');
    document.body.appendChild(el);
    return el;
  }

  function showPreview(link) {
    var href = link.getAttribute('href');
    if (!href) return;

    timer = setTimeout(function () {
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
            if (text.length > 200) text = text.substring(0, 200) + '\u2026';
            cache.set(href, text);
            displayPopup(link, text);
          })
          .catch(function () {});
      }
    }, 150);
  }

  function displayPopup(link, text) {
    if (!text) return;
    if (!popup) popup = createPopup();
    popup.textContent = text;

    var rect = link.getBoundingClientRect();
    popup.style.left = rect.left + window.scrollX + 'px';
    popup.style.top = (rect.bottom + window.scrollY + 6) + 'px';
    popup.classList.add('visible');
  }

  function hidePreview() {
    clearTimeout(timer);
    if (popup) popup.classList.remove('visible');
  }

  document.addEventListener('pointerenter', function (e) {
    var link = e.target.closest('a[href^="/notes/"]');
    if (link) showPreview(link);
  }, true);

  document.addEventListener('pointerleave', function (e) {
    var link = e.target.closest('a[href^="/notes/"]');
    if (link) hidePreview();
  }, true);

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hidePreview();
  });
})();
