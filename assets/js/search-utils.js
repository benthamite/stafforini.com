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
  return html.replace(/<(?!\/?mark\b)[^>]*>/gi, '');
}

function renderResult(r, section, currentQuery) {
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

// Export for ES module usage
if (typeof window !== 'undefined') {
  window._searchUtils = { escapeHtml: escapeHtml, highlightText: highlightText, sanitizeExcerpt: sanitizeExcerpt, renderResult: renderResult };
}
