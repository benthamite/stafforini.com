/**
 * Breakpoint toggle — shared viewport-width activation pattern.
 *
 * Used by toc.js and sidenotes.js to activate/deactivate features
 * based on viewport width, with debounced resize handling.
 */
function createBreakpointToggle(breakpoint, activateFn, deactivateFn, options) {
  var debounceMs = (options && options.debounceMs) || 150;
  var isActive = false;

  function activate() {
    if (isActive) return;
    isActive = true;
    activateFn();
  }

  function deactivate() {
    if (!isActive) return;
    isActive = false;
    deactivateFn();
  }

  function check() {
    if (window.innerWidth >= breakpoint) activate(); else deactivate();
  }

  var timer;
  window.addEventListener('resize', function () {
    clearTimeout(timer);
    timer = setTimeout(function () {
      deactivate();
      check();
    }, debounceMs);
  });

  check();

  return { activate: activate, deactivate: deactivate, check: check, isActive: function () { return isActive; } };
}

window.createBreakpointToggle = createBreakpointToggle;
