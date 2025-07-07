// Load GADM Level 1 boundaries
var gadm = ee.FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level1");

// Filter for Sri Lanka first
var sriLanka = gadm.filter(ee.Filter.eq('ADM0_NAME', 'Sri Lanka'));

// Now filter for Northern Province within Sri Lanka
var northern = sriLanka.filter(ee.Filter.eq('ADM1_NAME', 'Northern'));

// Display
Map.centerObject(northern, 8);
Map.addLayer(northern, {}, 'Northern Province (Sri Lanka)');


// Load Dynamic World for 2024
var dw = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
           .filterDate('2024-01-01', '2024-12-31')
           .filterBounds(northern)
           .select('label');

// Extract tree covers
var tree_cover = dw.map(function(img) {
  var tree_mask = img.eq(0)  // Trees
    .or(img.eq(1))          // Shrubland
    .or(img.eq(2))          // Grassland
    .or(img.eq(6))          // Cropland
    .or(img.eq(9));         // Wetlands
  return tree_mask.rename('tree_cover');
}).mean().clip(northern);


Map.addLayer(tree_cover, {min: 0, max: 1, palette: ['white', 'green']}, 'Tree Cover Mask');
Map.addLayer(northern, {}, 'Boundary');
Map.setOptions('SATELLITE');

// Export to Google Drive
Export.image.toDrive({
  image: tree_cover,
  description: 'Northern_Treecover_2024',
  fileNamePrefix: 'Northern_Treecover_2024',
  region: northern.geometry(),
  scale: 30,
  maxPixels: 1e13
});

