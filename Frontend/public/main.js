import '/geometry-web-worker.js'

document.body.style.height = "100%";








let safeSub = (portName, fn) => {
  if(app.ports[portName])
    app.ports[portName].subscribe(fn);
  else
    console.log("No port found", portName);
}

let safePub = (portName, data) => {
  if(app.ports[portName])
    app.ports[portName].send(data);
  else
    console.log("No port found", portName);
}

const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem('storage'))
  })

  app.ports.initMap.subscribe( () => {
    document.body.style.height = "100%";
      console.log("Beep Boop")
      var vecSource = new ol.source.Vector();
      window.vecSource = vecSource;
      var vecLayer = new ol.layer.Vector({source: vecSource});
    map = new ol.Map({
        target: 'map',
        layers: [
          new ol.layer.Tile({
            source: new ol.source.OSM()
          }),
          vecLayer
        ],
        view: view
      });
    setTimeout(() => {
      initTracking();
      
    }, 1000);
  });











const view = new ol.View({
  center: [0, 0],
  zoom: 2,
});


let map = null;

let initTracking = () => {
  const geolocation = new ol.Geolocation({
    // enableHighAccuracy must be set to true to have the heading value.
    trackingOptions: {
      enableHighAccuracy: true,
    },
    projection: view.getProjection(),
  });

  geolocation.setTracking(true);

  geolocation.on('change', function () {
    // el('accuracy').innerText = geolocation.getAccuracy() + ' [m]';
    // el('altitude').innerText = geolocation.getAltitude() + ' [m]';
    // el('altitudeAccuracy').innerText = geolocation.getAltitudeAccuracy() + ' [m]';
    // el('heading').innerText = geolocation.getHeading() + ' [rad]';
    // el('speed').innerText = geolocation.getSpeed() + ' [m/s]';
  });
  
  // handle geolocation error.
  geolocation.on('error', function (error) {
    const info = document.getElementById('info');
    info.innerHTML = error.message;
    info.style.display = '';
  });
  
  let fitted = false;

  const accuracyFeature = new ol.Feature();
  geolocation.on('change:accuracyGeometry', function () {
    accuracyFeature.setGeometry(geolocation.getAccuracyGeometry());
    if(!fitted)
    {
      view.fit(accuracyFeature.getGeometry());
      fitted = true;
    }
  });
  
  const positionFeature = new ol.Feature();
  positionFeature.setStyle(
    new ol.style.Style({
      image: new ol.style.Circle({
        radius: 6,
        fill: new ol.style.Fill({
          color: '#3399CC',
        }),
        stroke: new ol.style.Stroke({
          color: '#fff',
          width: 2,
        }),
      }),
    })
  );
  
  geolocation.on('change:position', function () {
    const coordinates = geolocation.getPosition();
    console.log(coordinates);

    safePub("locationChanged", { lat : coordinates[0], lon: coordinates[1] } );

    positionFeature.setGeometry(coordinates ? new ol.geom.Point(coordinates) : null);
    //if(accuracyFeature && accuracyFeature.getGeometry() && accuracyFeature.getGeometry().getType())
      //view.fit(accuracyFeature.getGeometry())
  });
  
  let lyr = new ol.layer.Vector({
    map: map,
    source: new ol.source.Vector({
      features: [accuracyFeature, positionFeature],
    }),
  });

  window.locationLayer = lyr;
}



  safeSub("upload", storage => {
        
        geometry_web_worker.promptAndProcessShapefile().then(features => 
        { 
            if(features && features.features.length)
            safePub("load", features.features[0]);
        });
  });

  safeSub("setShapes", shapes => {
      var gj = new ol.format.GeoJSON()
      for(var feat of shapes)
      {
          //TODO from geojson
            var f = gj.readFeature(feat);
            vecSource.addFeature(f);
      }
  });

  safeSub("zoom", loc => {
    view.setCenter([ loc.lat, loc.lon ]);
  });

  safeSub("store", data => {
    var oldKeys = window.localStorage.getItem("keys");

    if(!oldKeys)
      oldKeys = [];
    else
      oldKeys = JSON.parse(oldKeys);

    oldKeys.push(data.key);

    window.localStorage.setItem(data.key, JSON.stringify(data.value));

    window.localStorage.setItem("keys", JSON.stringify(oldKeys));
  });

  safeSub("load", data => {
    var oldKeys = window.localStorage.getItem("keys");

    if(!oldKeys)
      oldKeys = [];
    else
      oldKeys = JSON.parse(oldKeys);

    if(oldKeys.includes(data.key))
      safePub("loaded", JSON.parse(window.localStorage.getItem(data.key)))
    else
      safePub("loaded", {});
  });


  let initEventLayer = () => {
    if(window.eventLayer)
      return;
  
    let lyr = new ol.layer.Vector({
      map: map,
      source: new ol.source.Vector({
        features: [],
      }),
    });

    window.eventLayer = lyr;
  }

  safeSub("showEvents", events => {
    initEventLayer();
    window.eventLayer.getSource().clear();
    let lastEvt;
    events.map( evt => {
      let positionFeature = new ol.Feature();
      let fill = "#F99";

      switch(evt.type)
      {
        case "mark":
          fill = "#F99";
          break;
          
        case "start":
          fill = "#9F9";
          break;
          
        case "end":
          fill = "#99F";
          break;
          
        case "intersection":
          fill = "#FF9";
          break;
          
      }

      positionFeature.setGeometry(evt.location ? new ol.geom.Point([evt.location.lat, evt.location.lon]) : null);
      positionFeature.setStyle(
        new ol.style.Style({
          image: new ol.style.Circle({
            radius: 6,
            fill: new ol.style.Fill({
              color: fill,
            }),
            stroke: new ol.style.Stroke({
              color: '#f00',
              width: 2,
            }),
          }),
        })
      );
      window.eventLayer.getSource().addFeature(positionFeature);

      if(window.lastEvt)
      {
        var lineFeat = new ol.Feature();
        var coord1 = [window.lastEvt.location.lat, window.lastEvt.location.lon];
        var coord2 = [evt.location.lat, evt.location.lon];
        let ls = new ol.geom.LineString([coord1, coord2], 'XY');
        lineFeat.setGeometry(ls);
        //ls.appendCoordinate(coord1)
        //ls.appendCoordinate(coord2)
        console.log("Connecting", coord1, coord2);
        lineFeat.setStyle(
          new ol.style.Style({
            stroke: new ol.style.Stroke({
              color: '#f3f',
              width: 2,
            }),
          })
        );
        window.eventLayer.getSource().addFeature(lineFeat);

      }

      window.lastEvt = evt;
    });
  });

  safeSub("appendEvents", events => {
    initEventLayer();
    let lastEvt;
    events.map( evt => {
      let positionFeature = new ol.Feature();
      let fill = "#F99";

      switch(evt.type)
      {
        case "mark":
          fill = "#F99";
          break;
          
        case "start":
          fill = "#9F9";
          break;
          
        case "end":
          fill = "#99F";
          break;
          
        case "intersection":
          fill = "#FF9";
          break;
          
      }

      positionFeature.setGeometry(evt.location ? new ol.geom.Point([evt.location.lat, evt.location.lon]) : null);
      positionFeature.setStyle(
        new ol.style.Style({
          image: new ol.style.Circle({
            radius: 6,
            fill: new ol.style.Fill({
              color: fill,
            }),
            stroke: new ol.style.Stroke({
              color: '#f00',
              width: 2,
            }),
          }),
        })
      );
      window.eventLayer.getSource().addFeature(positionFeature);

      if(window.lastEvt)
      {
        var lineFeat = new ol.Feature();
        var coord1 = [window.lastEvt.location.lat, window.lastEvt.location.lon];
        var coord2 = [evt.location.lat, evt.location.lon];
        let ls = new ol.geom.LineString([coord1, coord2], 'XY');
        lineFeat.setGeometry(ls);
        //ls.appendCoordinate(coord1)
        //ls.appendCoordinate(coord2)
        console.log("Connecting", coord1, coord2);
        lineFeat.setStyle(
          new ol.style.Style({
            stroke: new ol.style.Stroke({
              color: '#f3f',
              width: 2,
            }),
          })
        );
        window.eventLayer.getSource().addFeature(lineFeat);

      }

      window.lastEvt = evt;
    });
  });


  safeSub("startTicking", () => {
    window.tickerTimer = setInterval(() => {
      safePub("tick", Math.floor( new Date() / 1000 ));
    }, 1000);
  });


  safeSub("stopTicking", () => {
    clearInterval(window.tickerTimer);
  });