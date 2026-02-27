// Shiny custom message handlers for gglite / AntV G2 charts
(function() {
  "use strict";

  // Store chart instances keyed by container ID
  var charts = {};

  // Handler: render a full chart
  Shiny.addCustomMessageHandler("g2-render", function(message) {
    var id = message.id;
    var config = message.config;

    // Destroy existing chart if present
    if (charts[id]) {
      charts[id].destroy();
      delete charts[id];
    }

    config.container = id;
    var chart = new G2.Chart();
    chart.options(config);
    chart.render();
    charts[id] = chart;
  });

  // Handler: update data only
  Shiny.addCustomMessageHandler("g2-update-data", function(message) {
    var id = message.id;
    var data = message.data;
    if (charts[id]) {
      charts[id].changeData(data);
    }
  });

  // Handler: destroy a chart
  Shiny.addCustomMessageHandler("g2-destroy", function(message) {
    var id = message.id;
    if (charts[id]) {
      charts[id].destroy();
      delete charts[id];
    }
  });
})();
