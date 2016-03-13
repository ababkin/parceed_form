function resultStates(data){

  var parsedData = $.parseJSON(data);
  // console.log(parsedData);

  var stateMap = {
    "Alabama": "us-al",
    "Alaska": "us-ak",
    "Arizona": "us-az",
    "Arkansas": "us-ar",
    "California": "us-ca",
    "Colorado": "us-co",
    "Connecticut": "us-ct",
    "Delaware": "us-de",
    "Florida": "us-fl",
    "Georgia": "us-ga",
    "Hawaii": "us-hi",
    "Idaho": "us-id",
    "Illinois": "us-il",
    "Indiana": "us-in",
    "Iowa": "us-ia",
    "Kansas": "us-ks",
    "Kentucky": "us-ky",
    "Louisiana": "us-la",
    "Maine": "us-me",
    "Maryland": "us-md",
    "Massachusetts": "us-ma",
    "Michigan": "us-mi",
    "Minnesota": "us-mn",
    "Mississippi": "us-ms",
    "Missouri": "us-mo",
    "Montana": "us-mt",
    "Nebraska": "us-ne",
    "Nevada": "us-nv",
    "New Hampshire": "us-nh",
    "New Jersey": "us-nj",
    "New Mexico": "us-nm",
    "New York": "us-ny",
    "North Carolina": "us-nc",
    "North Dakota": "us-nd",
    "Ohio": "us-oh",
    "Oklahoma": "us-ok",
    "Oregon": "us-or",
    "Pennsylvania": "us-pa",
    "Rhode Island": "us-ri",
    "South Carolina": "us-sc",
    "South Dakota": "us-sd",
    "Tennessee": "us-tn",
    "Texas": "us-tx",
    "Utah": "us-ut",
    "Vermont": "us-vt",
    "Virginia": "us-va",
    "Washington": "us-wa",
    "West Virginia": "us-wv",
    "Wisconsin": "us-wi",
    "Wyoming": "us-wy"
  }



  var mapData = $.map(parsedData, function(stateData){
    return {
      "hc-key": stateMap[stateData[0]],
      "value": stateData[1]
    };
  });

  // Initiate the chart
  $('#container').highcharts('Map', {

    title : {
        text : 'Programs map'
    },

    // subtitle : {
        // text : 'Source map: <a href="https://code.highcharts.com/mapdata/countries/us/us-all.js">United States of America</a>'
    // },

    mapNavigation: {
        enabled: true,
        buttonOptions: {
            verticalAlign: 'bottom',
            align: "right"
        }
    },

    colorAxis: {
        min: 0
    },

    series : [{
        data : mapData,
        mapData: Highcharts.maps['countries/us/us-all'],
        joinBy: 'hc-key',
        name: 'Programs',
        states: {
            hover: {
                color: '#BADA55'
            }
        },
        dataLabels: {
            enabled: true,
            format: '{point.name}'
        }
    }, {
        name: 'Separators',
        type: 'mapline',
        data: Highcharts.geojson(Highcharts.maps['countries/us/us-all'], 'mapline'),
        color: 'silver',
        showInLegend: false,
        enableMouseTracking: false
    }]
  });

}
