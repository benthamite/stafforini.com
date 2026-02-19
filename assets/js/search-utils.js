/* Shared search rendering utilities */

function escapeHtml(s) {
  var d = document.createElement('div');
  d.textContent = s;
  return d.innerHTML;
}

function highlightText(escaped, query) {
  if (!query) return escaped;
  var words = query.trim().split(/\s+/).filter(function(w) { return w.length > 0; });
  if (!words.length) return escaped;
  var pattern = words.map(function(w) { return w.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); }).join('|');
  return escaped.replace(new RegExp('(' + pattern + ')', 'gi'), '<mark>$1</mark>');
}

function sanitizeExcerpt(html) {
  // Strip all HTML tags except <mark>...</mark> (preserves Pagefind search highlighting)
  return html.replace(/<(?!\/?mark\b)[^>]*>/gi, '');
}

function renderResult(r, section, currentQuery) {
  // Works have work_author/work_title metadata (from Pagefind indexing); other pages use their title directly
  var label;
  if (r.meta.work_author && r.meta.work_title) {
    label = highlightText(escapeHtml(r.meta.work_author), currentQuery) + ', <em>' + highlightText(escapeHtml(r.meta.work_title), currentQuery) + '</em>';
  } else {
    label = highlightText(escapeHtml(r.meta.title || r.url), currentQuery);
  }
  var html = '<li><a href="' + escapeHtml(r.url) + '">' + label + '</a>';
  if (section === 'works') {
    if (r.meta.work_abstract) html += '<p class="search-excerpt">' + escapeHtml(r.meta.work_abstract) + '</p>';
  } else {
    if (r.excerpt) html += '<p class="search-excerpt">' + sanitizeExcerpt(r.excerpt) + '</p>';
  }
  html += '</li>';
  return html;
}

/**
 * Create keyboard navigation for search results.
 *
 * Arrow keys move a visual highlight through result links while focus stays
 * on the input.  Enter navigates to the highlighted result.
 *
 * @param {HTMLElement} resultsEl - The container that holds rendered results
 * @param {HTMLInputElement} input - The search input element
 * @returns {{ refresh: function }} Call refresh() after rendering new results
 */
function createKeyboardNav(resultsEl, input) {
  var activeIndex = -1;
  var items = [];

  function collectItems() {
    items = Array.from(resultsEl.querySelectorAll('.search-section-list a, a.search-more'));
    activeIndex = -1;
  }

  function highlightTarget(link) {
    var li = link.closest('li');
    return li || link;
  }

  function clearActive() {
    for (var i = 0; i < items.length; i++) {
      highlightTarget(items[i]).classList.remove('is-active');
    }
  }

  function setActive(idx) {
    clearActive();
    activeIndex = idx;
    if (idx >= 0 && items[idx]) {
      var target = highlightTarget(items[idx]);
      target.classList.add('is-active');
      target.scrollIntoView({ block: 'nearest' });
    }
  }

  input.addEventListener('keydown', function(e) {
    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (items.length === 0) return;
      setActive(activeIndex < items.length - 1 ? activeIndex + 1 : activeIndex);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (activeIndex > 0) {
        setActive(activeIndex - 1);
      } else if (activeIndex === 0) {
        setActive(-1);
      }
    } else if (e.key === 'Enter' && activeIndex >= 0 && items[activeIndex]) {
      e.preventDefault();
      items[activeIndex].click();
    }
  });

  return { refresh: collectItems };
}

// Export for ES module usage
if (typeof window !== 'undefined') {
  // Underscore prefix is a namespace convention (not "private"); consumed by header.html and search.html
  window._searchUtils = { escapeHtml: escapeHtml, highlightText: highlightText, sanitizeExcerpt: sanitizeExcerpt, renderResult: renderResult, createKeyboardNav: createKeyboardNav };
}
