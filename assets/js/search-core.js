/* Shared search logic for header overlay and full search page.
 *
 * Must match Hugo content sections. Update here if adding/removing sections.
 */

var SEARCH_DEBOUNCE_MS = 200;

var searchSections = [
  { key: 'notes', label: 'Notes' },
  { key: 'quotes', label: 'Quotes' },
  { key: 'works', label: 'Works' }
];

/**
 * Run a search query against Pagefind and invoke a callback with results.
 *
 * @param {string} query - The search string
 * @param {object} pagefind - The initialized Pagefind instance
 * @param {object} options
 * @param {number}   options.maxResultsPerSection - Max results per section (0 = unlimited)
 * @param {string}   [options.filterSection]      - If set, restrict to this section key
 * @param {function} options.onResults             - Callback receiving (html: string)
 * @param {function} options.getGeneration         - Returns current search generation counter
 * @param {function} options.incrementGeneration   - Increments and returns new generation counter
 */
async function runSearch(query, pagefind, options) {
  var gen = options.incrementGeneration();

  if (!query || !pagefind) {
    options.onResults('');
    return;
  }

  var utils = window._searchUtils;
  var maxResults = options.maxResultsPerSection || 0;
  var filterKey = options.filterSection;

  var activeSections = filterKey
    ? searchSections.filter(function(s) { return s.key === filterKey; })
    : searchSections;

  var html = '';
  for (var i = 0; i < activeSections.length; i++) {
    var sec = activeSections[i];
    var search = await pagefind.search(query, {
      filters: { section: [sec.key] }
    });

    if (gen !== options.getGeneration()) return;

    if (search.results.length === 0) continue;

    var items = maxResults > 0
      ? search.results.slice(0, maxResults)
      : search.results;
    var results = await Promise.all(items.map(function(r) { return r.data(); }));

    if (gen !== options.getGeneration()) return;

    html += '<div class="search-section">';
    if (maxResults > 0) {
      html += '<h3 class="search-section-heading">' + sec.label + '</h3>';
    } else {
      html += '<h3 class="search-section-heading">' + sec.label + ' (' + results.length + ')</h3>';
    }
    html += '<ul class="search-section-list">';
    for (var j = 0; j < results.length; j++) {
      html += utils.renderResult(results[j], sec.key, query);
    }
    html += '</ul>';
    if (maxResults > 0 && search.results.length > maxResults) {
      var moreUrl = '/search/?q=' + encodeURIComponent(query) + '&section=' + sec.key;
      html += '<a class="search-more" href="' + moreUrl + '">' + (search.results.length - maxResults) + ' more results</a>';
    }
    html += '</div>';
  }

  if (gen !== options.getGeneration()) return;

  if (!html) {
    html = '<p class="search-no-results">No results found.</p>';
  }

  options.onResults(html);
}

// Export for use in both header and search page scripts
if (typeof window !== 'undefined') {
  window._searchCore = {
    SEARCH_DEBOUNCE_MS: SEARCH_DEBOUNCE_MS,
    sections: searchSections,
    runSearch: runSearch
  };
}
