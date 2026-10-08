// Shiny output binding for gglite / AntV G2 charts
'use strict';

$(document).ready(function() {
  const g2OutputBinding = new Shiny.OutputBinding();
  Object.assign(g2OutputBinding, {
    find: function(scope) {
      return $(scope).find('.gglite-output');
    },
    // Tear down any chart or error message left from a previous render.
    _clear: function(el) {
      if (el._g2chart) {
        el._g2chart.destroy();
        el._g2chart = null;
      }
      el.innerHTML = '';
    },
    renderValue: function(el, data) {
      if (!data) return;
      this._clear(el);
      const ctor = Object.assign({}, data.ctor, { container: el.id });
      // spec arrives as a JSON string; evaluate it (not JSON.parse) so embedded
      // JS literals such as a tickMethod function survive the Shiny transport.
      const spec = typeof data.spec === 'string' ?
        new Function('return (' + data.spec + ')')() : data.spec;
      const chart = new G2.Chart(ctor);
      chart.options(spec);
      chart.render();
      el._g2chart = chart;
    },
    // Shiny applies the shiny-output-error[-validation] class to el, which
    // controls colour; we supply the message text in place of the chart.
    renderError: function(el, err) {
      console.error('gglite:', err.message);
      this._clear(el);
      el.textContent = err.message;
    }
  });

  Shiny.outputBindings.register(g2OutputBinding, 'gglite.g2');
});
