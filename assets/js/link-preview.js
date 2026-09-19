(function () {
  var HOVER_DEBOUNCE_MS = 150;
  var HIDE_DELAY_MS = 150; // allow crossing the gap between link and popup
  var POPUP_GAP = 6;
  var POPUP_EDGE_MARGIN = 8;
  var PREVIEW_MAX_LENGTH = 200; // note excerpts only; work abstracts remain complete
  var PREVIEWABLE_PREFIXES = ['/notes/', '/about/', '/contact/', '/works/'];

  var cache = new Map();
  var popup = null;
  var timer = null;
  var hideTimer = null;
  var activeLink = null;

  function previewUrl(link) {
    var href = link.getAttribute('href');
    if (!href || href.startsWith('#')) return null;
    var url;
    try { url = new URL(href, document.baseURI); } catch (e) { return null; }
    if (url.origin !== window.location.origin) return null;
    if (!PREVIEWABLE_PREFIXES.some(function (prefix) {
      return url.pathname.startsWith(prefix);
    })) return null;
    url.hash = '';
    return url.href;
  }

  function textAt(doc, selector) {
    var el = doc.querySelector(selector);
    return el ? el.textContent.trim() : '';
  }

  function extractPreview(doc) {
    // Inspect the fetched page, so redirects to another work or note work too.
    if (doc.querySelector('.work-header')) {
      var body = doc.querySelector('.work-body');
      var abstract = '';
      if (body) {
        body = body.cloneNode(true);
        var heading = body.querySelector('h2');
        if (heading) heading.remove();
        // textContent alone joins paragraphs and <br>-separated lines in minified HTML.
        body.querySelectorAll('br').forEach(function (br) {
          br.replaceWith(document.createTextNode('\n'));
        });
        body.querySelectorAll('p, li, blockquote, div').forEach(function (block) {
          block.appendChild(document.createTextNode('\n\n'));
        });
        abstract = body.textContent.trim();
      }
      return {
        title: textAt(doc, '.work-header h1'),
        author: textAt(doc, '.work-author'),
        details: textAt(doc, '.work-details').replace(/\s+/g, ' '),
        abstract: abstract
      };
    }
    var text = textAt(doc, '.note-body p');
    if (text.length > PREVIEW_MAX_LENGTH) text = text.substring(0, PREVIEW_MAX_LENGTH) + '\u2026';
    return { excerpt: text };
  }

  function cancelHide() {
    clearTimeout(hideTimer);
  }

  function createPopup() {
    var el = document.createElement('div');
    el.className = 'link-preview';
    el.setAttribute('role', 'tooltip');
    el.setAttribute('aria-hidden', 'true');
    el.addEventListener('pointerenter', cancelHide);
    el.addEventListener('pointerleave', scheduleHide);
    document.body.appendChild(el);
    return el;
  }

  function showPreview(link) {
    var href = previewUrl(link);
    if (!href) return;
    cancelHide();
    clearTimeout(timer);
    if (popup) {
      popup.classList.remove('visible');
      popup.setAttribute('aria-hidden', 'true');
    }
    activeLink = link;
    timer = setTimeout(function () {
      if (activeLink !== link) return;
      if (cache.has(href)) {
        displayPopup(link, cache.get(href));
        return;
      }
      fetch(href)
        .then(function (r) {
          if (!r.ok) throw new Error(r.status);
          return r.text();
        })
        .then(function (html) {
          var doc = new DOMParser().parseFromString(html, 'text/html');
          var preview = extractPreview(doc);
          cache.set(href, preview);
          if (activeLink === link) displayPopup(link, preview);
        })
        .catch(function () {
          // Do not cache failures: leaving and re-entering the link retries.
        });
    }, HOVER_DEBOUNCE_MS);
  }

  function displayPopup(link, preview) {
    if (!Object.keys(preview).some(function (key) { return preview[key]; })) return;
    if (!popup) popup = createPopup();
    popup.textContent = '';
    ['title', 'author', 'details', 'abstract', 'excerpt'].forEach(function (field) {
      if (!preview[field]) return;
      var p = document.createElement('p');
      p.className = 'link-preview-' + field;
      // Keep fetched HTML inert, including markup in bibliography abstracts.
      p.textContent = preview[field];
      popup.appendChild(p);
    });
    popup.scrollTop = 0;
    var rect = link.getBoundingClientRect();
    popup.style.left = POPUP_EDGE_MARGIN + 'px';
    popup.style.top = POPUP_EDGE_MARGIN + 'px';
    popup.classList.add('visible');
    popup.setAttribute('aria-hidden', 'false');
    var size = popup.getBoundingClientRect();
    var left = Math.max(POPUP_EDGE_MARGIN, Math.min(rect.left, window.innerWidth - size.width - POPUP_EDGE_MARGIN));
    var top = rect.bottom + POPUP_GAP;
    if (top + size.height > window.innerHeight - POPUP_EDGE_MARGIN) {
      top = rect.top - size.height - POPUP_GAP;
    }
    top = Math.max(POPUP_EDGE_MARGIN, Math.min(top, window.innerHeight - size.height - POPUP_EDGE_MARGIN));
    popup.style.left = left + 'px';
    popup.style.top = top + 'px';
  }

  function hidePreview() {
    activeLink = null;
    clearTimeout(timer);
    cancelHide();
    if (popup) {
      popup.classList.remove('visible');
      popup.setAttribute('aria-hidden', 'true');
    }
  }

  function scheduleHide() {
    clearTimeout(timer);
    cancelHide();
    hideTimer = setTimeout(hidePreview, HIDE_DELAY_MS);
  }

  function eventLink(e) {
    var el = e.target.nodeType === Node.ELEMENT_NODE ? e.target : e.target.parentElement;
    return el ? el.closest('a[href]') : null;
  }

  document.addEventListener('pointerover', function (e) {
    var link = eventLink(e);
    if (link && (!e.relatedTarget || !link.contains(e.relatedTarget))) showPreview(link);
  });
  document.addEventListener('pointerout', function (e) {
    var link = eventLink(e);
    if (link && link === activeLink && (!e.relatedTarget || !link.contains(e.relatedTarget))) scheduleHide();
  });
  document.addEventListener('focusin', function (e) {
    var link = eventLink(e);
    if (link) showPreview(link);
  });
  document.addEventListener('focusout', function (e) {
    var link = eventLink(e);
    if (link && link === activeLink) scheduleHide();
  });
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hidePreview();
  });
  // Fixed-position previews should not linger over unrelated content after scrolling.
  window.addEventListener('scroll', hidePreview);
  window.addEventListener('resize', hidePreview);
})();
