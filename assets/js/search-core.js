/* Shared search logic for header overlay and full search page.
 *
 * Must match Hugo content sections. Update here if adding/removing sections.
 */

var SEARCH_DEBOUNCE_MS = 200;
var DEFAULT_FULL_PAGE_RESULTS_PER_SECTION = 25;

var searchSections = [
  { key: 'notes', label: 'Notes' },
  { key: 'quotes', label: 'Quotes' },
  { key: 'works', label: 'Works' }
];

var searchResultStores = {};

function resetSearchStore(instanceId) {
  if (!instanceId) return;
  searchResultStores[instanceId] = {};
}

function storeSectionResults(instanceId, sectionKey, query, resultHandles, nextOffset, batchSize) {
  if (!instanceId) return;
  if (!searchResultStores[instanceId]) searchResultStores[instanceId] = {};
  searchResultStores[instanceId][sectionKey] = {
    query: query,
    resultHandles: resultHandles,
    nextOffset: nextOffset,
    batchSize: batchSize
  };
}

function renderMoreControl(sec, totalResults, renderedResults, query, options) {
  var remaining = totalResults - renderedResults;
  if (remaining <= 0) return '';

  if (options.showMoreButtons) {
    return '<button class="search-more" type="button" data-search-show-more data-search-section="' + sec.key + '">' + remaining + ' more results</button>';
  }

  var moreUrl = '/search/?q=' + encodeURIComponent(query) + '&section=' + sec.key;
  return '<a class="search-more" href="' + moreUrl + '">' + remaining + ' more results</a>';
}

function renderSectionHeading(sec, totalResults, showCounts) {
  var label = sec.label;
  if (showCounts) {
    label += ' (' + totalResults + ')';
  }
  return '<h3 class="search-section-heading">' + label + '</h3>';
}

/**
 * Run a search query against Pagefind and invoke a callback with results.
 *
 * @param {string} query - The search string
 * @param {object} pagefind - The initialized Pagefind instance
 * @param {object} options
 * @param {number}   options.maxResultsPerSection - Cap per section (0 = show all results)
 * @param {string}   [options.filterSection]      - If set, restrict to this section key
 * @param {boolean}  [options.showCounts]         - If true, show total Pagefind hits per section
 * @param {boolean}  [options.showMoreButtons]    - If true, render batched "more results" buttons
 * @param {function} options.onResults             - Callback receiving (html: string)
 * @param {function} options.getGeneration         - Returns current generation counter
 * @param {function} options.incrementGeneration   - Increments and returns new generation;
 *   the generation counter tracks which search invocation is current — when a
 *   newer search starts, earlier in-flight searches detect the mismatch and
 *   silently discard their stale results
 */
async function runSearch(query, pagefind, options) {
  var gen = options.incrementGeneration();
  resetSearchStore(options.instanceId);

  if (!query || !pagefind) {
    options.onResults('');
    return;
  }

  var utils = window._searchUtils;
  if (!utils) {
    options.onResults('<p class="search-no-results">Search is temporarily unavailable.</p>');
    return;
  }
  var maxResults = options.maxResultsPerSection != null ? options.maxResultsPerSection : 0;
  var filterKey = options.filterSection;
  var showCounts = !!options.showCounts;
  var showMoreButtons = !!options.showMoreButtons;

  var activeSections = filterKey
    ? searchSections.filter(function(s) { return s.key === filterKey; })
    : searchSections;

  try {
    var sectionHtml = await Promise.all(activeSections.map(async function(sec) {
      var search = await pagefind.search(query, {
        filters: { section: [sec.key] }
      });

      if (gen !== options.getGeneration()) return '';

      if (search.results.length === 0) return '';

      var items = maxResults > 0
        ? search.results.slice(0, maxResults)
        : search.results;
      var results = await Promise.all(items.map(function(r) { return r.data(); }));

      if (gen !== options.getGeneration()) return '';

      if (showMoreButtons && maxResults > 0 && search.results.length > results.length) {
        storeSectionResults(options.instanceId, sec.key, query, search.results, results.length, maxResults);
      }

      var html = '<div class="search-section" data-search-section="' + sec.key + '">';
      html += renderSectionHeading(sec, search.results.length, showCounts);
      html += '<ul class="search-section-list">';
      for (var j = 0; j < results.length; j++) {
        html += utils.renderResult(results[j], sec.key, query);
      }
      html += '</ul>';
      html += renderMoreControl(sec, search.results.length, results.length, query, {
        showMoreButtons: showMoreButtons
      });
      html += '</div>';
      return html;
    }));

    if (gen !== options.getGeneration()) return;

    var html = sectionHtml.join('');
    if (!html) {
      html = '<p class="search-no-results">No results found.</p>';
    }

    options.onResults(html);
  } catch (err) {
    if (gen === options.getGeneration()) {
      options.onResults('<p class="search-no-results">Search encountered an error. Please try again.</p>');
    }
  }
}

async function showMoreResults(instanceId, sectionKey, options) {
  var utils = window._searchUtils;
  var store = searchResultStores[instanceId] && searchResultStores[instanceId][sectionKey];
  if (!utils || !store) return;

  var button = options.button;
  var resultsEl = options.resultsEl;
  var start = store.nextOffset;
  var end = Math.min(start + store.batchSize, store.resultHandles.length);
  var items = store.resultHandles.slice(start, end);

  button.disabled = true;
  button.textContent = 'Loading...';

  try {
    var results = await Promise.all(items.map(function(r) { return r.data(); }));
    var html = '';
    for (var i = 0; i < results.length; i++) {
      html += utils.renderResult(results[i], sectionKey, store.query);
    }

    var sectionEl = resultsEl.querySelector('.search-section[data-search-section="' + sectionKey + '"]');
    var listEl = sectionEl && sectionEl.querySelector('.search-section-list');
    if (!listEl) {
      button.disabled = false;
      button.textContent = 'Show more';
      return;
    }

    listEl.insertAdjacentHTML('beforeend', html);
    store.nextOffset = end;

    var remaining = store.resultHandles.length - end;
    if (remaining > 0) {
      button.disabled = false;
      button.textContent = remaining + ' more results';
    } else {
      button.remove();
    }

    if (options.onResultsChanged) options.onResultsChanged();
  } catch (err) {
    button.disabled = false;
    button.textContent = 'Show more';
  }
}

// Export for use in both header and search page scripts
if (typeof window !== 'undefined') {
  window._searchCore = {
    SEARCH_DEBOUNCE_MS: SEARCH_DEBOUNCE_MS,
    DEFAULT_FULL_PAGE_RESULTS_PER_SECTION: DEFAULT_FULL_PAGE_RESULTS_PER_SECTION,
    sections: searchSections,
    runSearch: runSearch,
    showMoreResults: showMoreResults
  };
}
