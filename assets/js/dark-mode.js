// Two data attributes on <html>:
//   data-theme      — resolved visual theme ("light" or "dark"), used by CSS for styling
//   data-theme-pref — user's preference ("light", "dark", or "system"), used by CSS for icon state
(function () {
  var toggle = document.querySelector('.dark-mode-toggle');
  if (!toggle) return;

  var validPrefs = ['light', 'dark', 'system'];
  var cycle = { light: 'dark', dark: 'system', system: 'light' };

  function applyPref(pref) {
    if (validPrefs.indexOf(pref) === -1) pref = 'system';
    var theme = pref === 'system'
      ? (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light')
      : pref;
    document.documentElement.setAttribute('data-theme', theme);
    document.documentElement.setAttribute('data-theme-pref', pref);
    try {
      if (pref === 'system') {
        // Removing the key (rather than storing "system") signals "follow OS preference"
        // to the inline script in head.html that runs before first paint
        localStorage.removeItem('theme');
      } else {
        localStorage.setItem('theme', pref);
      }
    } catch (e) {
      if (typeof console !== 'undefined') console.warn('Theme preference not saved:', e);
    }
  }

  toggle.addEventListener('click', function () {
    var pref = document.documentElement.getAttribute('data-theme-pref') || 'system';
    if (validPrefs.indexOf(pref) === -1) pref = 'system';
    applyPref(cycle[pref]);
  });

  // Update resolved theme when system preference changes
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {
    var pref = document.documentElement.getAttribute('data-theme-pref') || 'system';
    if (pref === 'system') {
      applyPref('system');
    }
  });
})();
