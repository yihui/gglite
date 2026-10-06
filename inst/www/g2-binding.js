// Shiny output binding for gglite / AntV G2 charts
'use strict';

$(document).ready(function() {
  const g2OutputBinding = new Shiny.OutputBinding();
  Object.assign(g2OutputBinding, {
    find: function(scope) {
      return $(scope).find('.gglite-output');
    },
    renderValue: function(el, data) {
      if (!data) return;
      if (el._g2chart) {
        el._g2chart.destroy();
        el._g2chart = null;
      }
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
    renderError: function(el, err) {
      console.error('gglite:', err.message);
    }
  });

  Shiny.outputBindings.register(g2OutputBinding, 'gglite.g2');
});
