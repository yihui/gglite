// gglite: patch G2 theme defaults — larger fonts (~25%), visible grid lines
(function() {
  // Deep-merge src keys into dest (src wins); returns new object
  function merge(dest, src) {
    var out = Object.assign({}, dest);
    Object.keys(src).forEach(function(k) {
      var sv = src[k], dv = dest ? dest[k] : undefined;
      if (sv && typeof sv === 'object' && !Array.isArray(sv) &&
          dv && typeof dv === 'object') {
        out[k] = merge(dv, sv);
      } else {
        out[k] = sv;
      }
    });
    return out;
  }

  var overrides = {
    axis: {
      labelFontSize: 15,
      titleFontSize: 15,
      gridStrokeOpacity: 0.25
    },
    label:       { fontSize: 15 },
    innerLabel:  { fontSize: 15 },
    legendCategory: {
      itemLabelFontSize: 15,
      itemValueFontSize: 15
    },
    point: {
      point:   { r: 3.75 },
      hollow:  { r: 3.75 },
      plus:    { r: 3.75 },
      diamond: { r: 3.75 }
    },
    text: { text: { fontSize: 15 } }
  };

  // Override each built-in G2 theme factory with our enhanced version
  ['Classic', 'ClassicDark', 'Light', 'Dark', 'Academy'].forEach(function(name) {
    var orig = G2[name];
    if (typeof orig !== 'function') return;
    var key = 'theme:' + name.charAt(0).toLowerCase() + name.slice(1);
    G2.register(key, function(options) {
      return merge(orig(options), overrides);
    });
  });
})();
