/**
 * Search instance factory — shared initialization for header overlay and search page.
 *
 * Creates a Pagefind search instance with debounced query execution, generation
 * tracking (to discard stale results), and keyboard navigation.
 *
 * @param {HTMLInputElement} inputEl - The search input element
 * @param {HTMLElement} resultsEl - The container for rendered results
 * @param {object} [options]
 * @param {number} [options.maxResultsPerSection] - Cap per section (0 = show all)
 * @param {boolean} [options.showCounts] - Show total hits in section headings
 * @param {boolean} [options.showMoreButtons] - Fetch additional results in batches
 * @returns {{ initPagefind: function, handleInput: function(query, filterSection) }}
 */
var nextSearchInstanceId = 0;

window.createSearchInstance = function (inputEl, resultsEl, options) {
  var debounceTimer;
  var searchGeneration = 0;
  var core = window._searchCore;
  var utils = window._searchUtils;

  if (!core || !utils) return null;

  var nav = utils.createKeyboardNav(resultsEl, inputEl);
  var pagefind = null;
  var instanceId = 'search-' + (++nextSearchInstanceId);
  // Use != null (not !==) to allow 0 as a valid value (meaning "show all results").
  var maxResults = (options && options.maxResultsPerSection != null) ? options.maxResultsPerSection : 0;
  var initPromise = null;

  function initPagefind() {
    if (initPromise) return initPromise;
    initPromise = import('/pagefind/pagefind.js').then(function (module) {
      pagefind = module;
      return pagefind.init().then(function () {
        return pagefind;
      });
    }).catch(function (err) {
      initPromise = null; // allow retry on transient failures
      throw err;
    });
    return initPromise;
  }

  function runQuery(query, filterSection) {
    core.runSearch(query, pagefind, {
      instanceId: instanceId,
      maxResultsPerSection: maxResults,
      filterSection: filterSection || undefined,
      showCounts: !!(options && options.showCounts),
      showMoreButtons: !!(options && options.showMoreButtons),
      onResults: function (html) { resultsEl.innerHTML = html; nav.refresh(); },
      getGeneration: function () { return searchGeneration; },
      incrementGeneration: function () { return ++searchGeneration; }
    });
  }

  /**
   * Debounced search — call this from an input event handler.
   * Waits for Pagefind to be initialized before running the query.
   *
   * @param {string} query - The search string
   * @param {string} [filterSection] - Optional section key to filter by
   */
  function handleInput(query, filterSection) {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(function () {
      var ready = initPromise || initPagefind();
      ready.then(function () { runQuery(query, filterSection); });
    }, core.SEARCH_DEBOUNCE_MS);
  }

  resultsEl.addEventListener('click', function(e) {
    var button = e.target.closest('[data-search-show-more]');
    if (!button || !resultsEl.contains(button)) return;
    e.preventDefault();
    core.showMoreResults(instanceId, button.getAttribute('data-search-section'), {
      button: button,
      resultsEl: resultsEl,
      onResultsChanged: nav.refresh
    });
  });

  return {
    initPagefind: initPagefind,
    handleInput: handleInput
  };
};
