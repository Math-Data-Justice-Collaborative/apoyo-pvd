
$(window).on("load", function () {
  $("#search-input").on("keyup", function () {
    var e = $(this).val().toLowerCase();
    $(".chapter-container").filter(function () {
      $(this).toggle($(this).text().toLowerCase().indexOf(e) > -1);
    });
  }),
    $(window).on("scroll", function () {
      var e = !1;
      $(".chapter-container:visible").each(function () {
        isElementInView(this) &&
          !e &&
          ($(".chapter-container")
            .removeClass("in-focus")
            .addClass("out-focus"),
          $(this).addClass("in-focus").removeClass("out-focus"),
          (e = !0));
      });
    });
  const e = [];
  let a = new SpeechSynthesisUtterance();
  var t = {},
    o =
      '"https://docs.google.com/spreadsheets/d/1cyi3hGwfjUDE6PScNv3Vj8fHOS9MTb0H8hLAr6BH6Kw/edit#gid=0"',
    r = "AIzaSyBwUXweqtDod91Xxg7yAjxYUQIH0K7wp8c";
  function n(e) {
    return t[constants[e]];
  }
  function s(e, a) {
    return (e = n(e)) && "" !== e.trim() ? e : a;
  }
  function l(r, l) {
    var c;
    !(function (e) {
      for (var a in e) {
        var i = e[a];
        t[i.Setting] = i.Customize;
      }
    })(r),
      (document.title = n("_mapTitle")),
      $("#header").append("<h1>" + (n("_mapTitle") || "") + "</h1>"),
      $("#header").append("<h2>" + (n("_mapSubtitle") || "") + "</h2>"),
      $("#search-input").on("keyup", function () {
        var e = $(this).val().toLowerCase();
        $(".chapter-container").filter(function () {
          $(this).toggle($(this).text().toLowerCase().indexOf(e) > -1);
        });
      }),
      n("_mapLogo")
        ? ($("#logo").append('<img src="' + n("_mapLogo") + '" />'),
          $("#top").css("height", "150px"))
        : ($("#logo").css("display", "none"),
          $("#header").css("padding-top", "25px")),
      (c = s("_tileProvider", "Stamen.TonerLite")),
      L.tileLayer.provider(c, { maxZoom: 18 }).addTo(map),
      "off" !== n("_zoomControls") &&
        L.control.zoom({ position: n("_zoomControls") }).addTo(map);
    var p,
      d,
      h,
      u,
      f,
      g,
      v,
      m = [],
      y = function (e) {
        for (var a = 0; a < m.length; a++)
          m[a] &&
            m[a]._icon &&
            ((m[a]._icon.className = m[a]._icon.className.replace(
              " marker-active",
              ""
            )),
            a == e && (m[e]._icon.className += " marker-active"));
      },
      x = [],
      w = 0;
    for (i in l) {
      var b = l[i];
      if (
        (console.log(l),
        isNaN(parseFloat(b.Latitude)) || isNaN(parseFloat(b.Longitude)))
      )
        m.push(null);
      else {
        var C = parseFloat(b.Latitude),
          k = parseFloat(b.Longitude);
        (w += 1),
          m.push(
            L.marker([C, k], {
              icon: L.ExtraMarkers.icon({
                icon: "fa-number",
                number:
                  "Numbered" === b.Marker
                    ? w
                    : "Plain" === b.Marker
                    ? ""
                    : b.Marker,
                markerColor: b["Marker Color"] || "blue",
              }),
              opacity: "Hidden" === b.Marker ? 0 : 0.9,
              interactive: "Hidden" !== b.Marker,
            })
          );
      }
      var _,
        O,
        S,
        T,
        N = $("<div></div>", {
          id: "container" + i,
          class: "chapter-container",
        }),
        M = null,
        P = null,
        A = null,
        H = null;
      (O = "Media Link"), (S = "Services Media Credit"), (T = "Media Link 2");
      var j;
      (j = b[(_ = "Media Credit")]
        ? $("<a>", {
            text: b[_],
            href: b["Media Credit Link"],
            target: "_blank",
            class: "source",
          })
        : $("<div>", { text: b[_], class: "source" })),
        b[O] &&
          b[O].indexOf("youtube.com/") > -1 &&
          ((M = $("<iframe></iframe>", {
            src: b[O],
            width: "100%",
            height: "100%",
            frameborder: "0",
            allow: "autoplay; encrypted-media",
            allowfullscreen: "allowfullscreen",
          })),
          (P = $("<div></div>", { class: "img-container" })
            .append(M)
            .after(j)));
      var J =
        {
          jpg: "img",
          jpeg: "img",
          png: "img",
          tiff: "img",
          gif: "img",
          mp3: "audio",
          ogg: "audio",
          wav: "audio",
        }[b[O] ? b[O].split(".").pop().toLowerCase() : ""] || "img";
      J &&
        ((M = $("<" + J + ">", {
          src: b[O],
          controls: "audio" === J ? "controls" : "",
          alt: b.Chapter,
        })),
        "yes" === n("_enableLightbox") &&
          "img" === J &&
          (M = $("<a></a>", {
            "data-lightbox": b[O],
            href: b[O],
            "data-title": b.Chapter,
            "data-alt": b.Chapter,
          }).append(M)),
        (P = $("<div></div", { class: J + "-container" })
          .append(M)
          .after(j)));
      var z;
      (z = b[S]
        ? $("<a>", {
            text: b[S],
            href: b["Services Media Credit Link"],
            target: "_blank",
            class: "source",
          })
        : $("<div>", { text: b[S], class: "source" })),
        b[T] &&
          b[T].indexOf("youtube.com/") > -1 &&
          ((M = $("<iframe></iframe>", {
            src: b[T],
            width: "100%",
            height: "100%",
            frameborder: "0",
            allow: "autoplay; encrypted-media",
            allowfullscreen: "allowfullscreen",
          })),
          (H = $("<div></div>", { class: "img-container" })
            .append(A)
            .after(z)));
      var E =
        {
          jpg: "img",
          jpeg: "img",
          png: "img",
          tiff: "img",
          gif: "img",
          mp3: "audio",
          ogg: "audio",
          wav: "audio",
        }[b[O] ? b[O].split(".").pop().toLowerCase() : ""] || "img";
      E &&
        ((A = $("<" + E + ">", {
          src: b[T],
          controls: "audio" === E ? "controls" : "",
          alt: b.Chapter,
        })),
        "yes" === n("_enableLightbox") &&
          "img" === E &&
          (A = $("<a></a>", {
            "data-lightbox": b[T],
            href: b[T],
            "data-title": b.Chapter,
            "data-alt": b.Chapter,
          }).append(A)),
        (H = $("<div></div", { class: E + "-container" })
          .append(A)
          .after(z))),
        e.push(b.Descripcion),
        console.log(b, "xxxxxxxxxxxxx"),
        N.append('<p class="chapter-header">' + b.Resource + "</p>")
          .append('<p class="chapter-address">' + b.Address + "</p>")
          .append('<p class="chapter-phone">' + b["Phone Number"] + "</p>")
          .append(M && b[O] ? P : "")
          .append(M ? j : "")
          .append(A && b[T] ? H : "")
          .append(A ? z : "")
          .append('<h2 class="translate-title"> Descripción </h2>')
          .append(
            `<button class='listen listen-${i} ' ><span>Escucha</span> <svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 24 24"><path d="M6 7l8-5v20l-8-5v-10zm-6 10h4v-10h-4v10zm20.264-13.264l-1.497 1.497c1.847 1.783 2.983 4.157 2.983 6.767 0 2.61-1.135 4.984-2.983 6.766l1.498 1.498c2.305-2.153 3.735-5.055 3.735-8.264s-1.43-6.11-3.736-8.264zm-.489 8.264c0-2.084-.915-3.967-2.384-5.391l-1.503 1.503c1.011 1.049 1.637 2.401 1.637 3.888 0 1.488-.623 2.841-1.634 3.891l1.503 1.503c1.468-1.424 2.381-3.309 2.381-5.394z"/></svg></button>`
          )
          .append(`<button class='stop stop-${i}' >Parar Escuchando</button>`)
          .append('<p class="description">' + b.Descripcion + "</p>")
          .append('<h2 class="translate-title">English Translation</h2>')
          .append('<p class="description">' + b.Description + "</p>"),
        $("#contents").append(N);
    }
    for (
      e.forEach((e, t) => {
        $(`.stop-${t}`).hide(),
          $(`.listen-${t}`).on("click", function () {
            (a.text = e),
              (a.lang = "es"),
              console.log(t, a.text),
              speechSynthesis.speak(a),
              $(`.stop-${t}`).show();
          }),
          $(`.stop-${t}`).on("click", function () {
            $(`.stop-${t}`).hide(), speechSynthesis.cancel();
          });
      }),
        void 0,
        f = void 0,
        void 0,
        v = void 0,
        u = $(".leaflet-control-attribution")[0].innerHTML,
        f =
          'View <a href="' +
          (o || "./csv/Chapters.csv") +
          '" target="_blank">data</a>',
        g = n("_authorName"),
        v = n("_authorURL"),
        g && v
          ? (v.indexOf("@") > 0 && (v = "mailto:" + v),
            (f += ' by <a href="' + v + '">' + g + "</a> | "))
          : (f += g ? " by " + g + " | " : " | "),
        f += 'View <a href="' + n("_githubRepo") + '">code</a>',
        n("_codeCredit") && (f += " by " + n("_codeCredit")),
        f += " with ",
        $(".leaflet-control-attribution")[0].innerHTML = f + u,
        imgContainerHeight = parseInt(n("_imgContainerHeight")),
        imgContainerHeight > 0 &&
          $(".img-container").css({
            height: imgContainerHeight + "px",
            "max-height": imgContainerHeight + "px",
          }),
        x[0] = -100,
        i = 1;
      i < l.length;
      i++
    )
      x[i] = x[i - 1] + $("div#container" + (i - 1)).height() + 70;
    x.push(Number.MAX_VALUE),
      $("div#contents").scroll(function () {
        var e = $(this).scrollTop();
        e < 200 && $("#title").css("opacity", 1 - Math.min(1, e / 100));
        for (var a = 0; a < x.length - 1; a++)
          if (e >= x[a] && e < x[a + 1] - 140 && p != a) {
            (location.hash = a + 1),
              $(".chapter-container").removeClass("in-focus"),
              $("div#container" + a)
                .addClass("in-focus")
                .removeClass("out-focus"),
              y((p = a)),
              map.hasLayer(d) && map.removeLayer(d),
              map.hasLayer(h) && map.removeLayer(h);
            var t = l[a];
            if (t.Overlay) {
              var i = parseFloat(t["Overlay Transparency"]) || 1,
                o = t.Overlay;
              "geojson" === o.split(".").pop()
                ? $.getJSON(o, function (e) {
                    d = L.geoJson(e, {
                      style: function (e) {
                        return {
                          fillColor: e.properties.fillColor || "#ffffff",
                          weight: e.properties.weight || 1,
                          opacity: e.properties.opacity || i,
                          color: e.properties.color || "#cccccc",
                          fillOpacity: e.properties.fillOpacity || 0.5,
                        };
                      },
                    }).addTo(map);
                  })
                : (d = L.tileLayer(t.Overlay, { opacity: i }).addTo(map));
            }
            if (
              (t["GeoJSON Overlay"] &&
                $.getJSON(t["GeoJSON Overlay"], function (e) {
                  var a = {};
                  if (t["GeoJSON Feature Properties"]) {
                    var i = t["GeoJSON Feature Properties"].split(";");
                    for (var o in ((a = {}), i))
                      2 === i[o].split(":").length &&
                        (a[i[o].split(":")[0].trim()] = i[o]
                          .split(":")[1]
                          .trim());
                  }
                  h = L.geoJson(e, {
                    style: function (e) {
                      return {
                        fillColor:
                          e.properties.fillColor || a.fillColor || "#ffffff",
                        weight: e.properties.weight || a.weight || 1,
                        opacity: e.properties.opacity || a.opacity || 0.5,
                        color: e.properties.color || a.color || "#cccccc",
                        fillOpacity:
                          e.properties.fillOpacity || a.fillOpacity || 0.5,
                      };
                    },
                  }).addTo(map);
                }),
              t.Latitude && t.Longitude)
            ) {
              var r = t.Zoom ? t.Zoom : 15;
              map.flyTo([t.Latitude, t.Longitude], r, {
                animate: !0,
                duration: 2,
              });
            }
            break;
          }
      }),
      $("#contents").append(
        "       <div id='space-at-the-bottom'>         <a href='#top'>            <i class='fa fa-chevron-up'></i></br>           <small>Top</small>          </a>       </div>     "
      ),
      $("<style>")
        .prop("type", "text/css")
        .html(
          "      #narration, #title {        background-color: " +
            s("_narrativeBackground", "white") +
            ";         color: " +
            s("_narrativeText", "black") +
            ";       }      a, a:visited, a:hover {        color: " +
            s("_narrativeLink", "blue") +
            "       }      .in-focus {        background-color: " +
            s("_narrativeActive", "#f0f0f0") +
            "       }"
        )
        .appendTo("head"),
      (endPixels = parseInt(n("_pixelsAfterFinalChapter"))),
      endPixels > 100 &&
        $("#space-at-the-bottom").css({
          height: endPixels / 2 + "px",
          "padding-top": endPixels / 2 + "px",
        });
    var F = [];
    for (i in m)
      m[i] &&
        (m[i].addTo(map),
        (m[i]._pixelsAbove = x[i]),
        m[i].on("click", function () {
          var e = parseInt($(this)[0]._pixelsAbove) + 5;
          $("div#contents").animate({ scrollTop: e + "px" });
        }),
        F.push(m[i].getLatLng()));
    if (
      (map.fitBounds(F),
      $("#map, #narration, #title").css("visibility", "visible"),
      $("div.loader").css("visibility", "hidden"),
      $("div#container0").addClass("in-focus"),
      $("div#contents").animate({ scrollTop: "1px" }),
      parseInt(location.hash.substr(1)))
    ) {
      var I = parseInt(location.hash.substr(1)) - 1;
      $("#contents").animate(
        { scrollTop: $("#container" + I).offset().top },
        2e3
      );
    }
    var D = n("_googleAnalytics");
    if (D && D.length >= 10) {
      var G = document.createElement("script");
      function U() {
        dataLayer.push(arguments);
      }
      G.setAttribute("src", "https://www.googletagmanager.com/gtag/js?id=" + D),
        document.head.appendChild(G),
        (window.dataLayer = window.dataLayer || []),
        U("js", new Date()),
        U("config", D);
    }
  }
  $.get("csv/Options.csv", function (e) {
    $.get("csv/Chapters.csv", function (a) {
      l($.csv.toObjects(e), $.csv.toObjects(a));
    }).fail(function (e) {
      alert("Found Options.csv, but could not read Chapters.csv");
    });
  }).fail(function (e) {
    var a = function (e) {
      return Papa.parse(Papa.unparse(e[0].values), { header: !0 }).data;
    };
    if (o)
      if (r) {
        var t = "https://sheets.googleapis.com/v4/spreadsheets/",
          i = o.split("/d/")[1].split("/")[0];
        $.when(
          $.getJSON(t + i + "/values/Options?key=" + r),
          $.getJSON(t + i + "/values/Chapters?key=" + r)
        ).then(function (e, t) {
          l(a(e), a(t));
        });
      } else
        alert(
          "You load data from a Google Sheet, you need to add a free Google API key"
        );
    else alert("You need to specify a valid Google Sheet (googleDocURL)");
  });
});

$(window).on("load",(function(){$("#search-input").on("keyup",(function(){var e=$(this).val().toLowerCase();$(".chapter-container").filter((function(){$(this).toggle($(this).text().toLowerCase().indexOf(e)>-1)}))}));const e=[];let t=new SpeechSynthesisUtterance;var a={},o="https://docs.google.com/spreadsheets/d/1GkdZoHTxtHV6YKbDLwZ9v9ukU6asVE6YNo2RobFmjRM/edit#gid=0",r="AIzaSyBwUXweqtDod91Xxg7yAjxYUQIH0K7wp8c";function n(e){return a[constants[e]]}function s(e,t){return(e=n(e))&&""!==e.trim()?e:t}function l(r,l){var p;!function(e){for(var t in e){var i=e[t];a[i.Setting]=i.Customize}}(r),document.title=n("_mapTitle"),$("#header").append("<h1>"+(n("_mapTitle")||"")+"</h1>"),$("#header").append("<h2>"+(n("_mapSubtitle")||"")+"</h2>"),n("_mapLogo")?($("#logo").append('<img src="'+n("_mapLogo")+'" />'),$("#top").css("height","150px")):($("#logo").css("display","none"),$("#header").css("padding-top","25px")),p=s("_tileProvider","Stamen.TonerLite"),L.tileLayer.provider(p,{maxZoom:18}).addTo(map),"off"!==n("_zoomControls")&&L.control.zoom({position:n("_zoomControls")}).addTo(map);var c,d,h,u,f,g,v,m=[],y=function(e){for(var t=0;t<m.length;t++)m[t]&&m[t]._icon&&(m[t]._icon.className=m[t]._icon.className.replace(" marker-active",""),t==e&&(m[e]._icon.className+=" marker-active"))},b=[],x=0;for(i in l){var w=l[i];if(console.log(l),isNaN(parseFloat(w.Latitude))||isNaN(parseFloat(w.Longitude)))m.push(null);else{var C=parseFloat(w.Latitude),_=parseFloat(w.Longitude);x+=1,m.push(L.marker([C,_],{icon:L.ExtraMarkers.icon({icon:"fa-number",number:x,markerColor:w["Marker Color"]||"blue"}),opacity:"Hidden"===w.Marker?0:.9,interactive:"Hidden"!==w.Marker}))}var k,O,T,S,N=$("<div></div>",{id:"container"+i,class:"chapter-container"}),M=null,A=null,H=null,P=null;O="Media Link",T="Services Media Credit",S="Media Link 2";var F,J={jpg:"img",jpeg:"img",png:"img",tiff:"img",gif:"img",mp3:"audio",ogg:"audio",wav:"audio"};F=w[k="Media Credit"]?$("<a>",{text:w[k],href:w["Media Credit Link"],target:"_blank",class:"source"}):$("<div>",{text:w[k],class:"source"});var j=J[w[O]?w[O].split(".").pop().toLowerCase():""]||"img";if(j&&(M=$("<"+j+">",{src:w[O],controls:"audio"===j?"controls":"",alt:w.Chapter}),"yes"===n("_enableLightbox")&&"img"===j&&(M=$("<a></a>",{"data-lightbox":w[O],href:w[O],"data-title":w.Chapter,"data-alt":w.Chapter}).append(M)),A=$("<div></div",{class:j+"-container"}).append(M).after(F)),w[O]&&w[O].indexOf("youtube.com/")>-1){-1!=(G=(E=w[O].split("v=")[1]).indexOf("&"))&&(E=E.substring(0,G));var z="https://www.youtube.com/embed/"+E+"/";M=$("<iframe></iframe>",{src:z,width:"100%",height:"100%",frameborder:"0",allow:"autoplay; encrypted-media",allowfullscreen:"allowfullscreen"}),A=$("<div></div>",{class:"img-container"}).append(M).after(F)}var D;D=w[T]?$("<a>",{text:w[T],href:w["Services Media Credit Link"],target:"_blank",class:"source"}):$("<div>",{text:w[T],class:"source"});var E,G,I=J[w[S]?w[S].split(".").pop().toLowerCase():""]||"img";I&&(H=$("<"+I+">",{src:w[S],controls:"audio"===I?"controls":"",alt:w.Chapter}),"yes"===n("_enableLightbox")&&"img"===I&&(H=$("<a></a>",{"data-lightbox":w[S],href:w[S],"data-title":w.Chapter,"data-alt":w.Chapter}).append(H)),P=$("<div></div",{class:I+"-container"}).append(H).after(D)),w[S]&&w[S].indexOf("youtube.com/")>-1&&(console.log("Will this thing display"),-1!=(G=(E=w[S].split("v=")[1]).indexOf("&"))&&(E=E.substring(0,G)),z="https://www.youtube.com/embed/"+E+"/",H=$("<iframe></iframe>",{src:z,width:"100%",height:"100%",frameborder:"0",allow:"autoplay; encrypted-media",allowfullscreen:"allowfullscreen"}),P=$("<div></div>",{class:"img-container"}).append(H).after(D)),e.push(w.Descripcion),console.log(M&&w[O]?A:"no","xxxxxxxxxxxxx"),N.append('<p class="chapter-header">'+w.Resource+"</p>").append('<p class="chapter-address">'+w.Address+"</p>").append('<p class="chapter-phone">'+w["Phone Number"]+"</p>").append(M&&w[O]?A:"").append(M?F:"").append(H&&w[S]?P:"").append(H?D:"").append('<h2 class="translate-title"> Descripción </h2>').append(`<button class='listen listen-${i} ' ><span>Escucha</span> <svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 24 24"><path d="M6 7l8-5v20l-8-5v-10zm-6 10h4v-10h-4v10zm20.264-13.264l-1.497 1.497c1.847 1.783 2.983 4.157 2.983 6.767 0 2.61-1.135 4.984-2.983 6.766l1.498 1.498c2.305-2.153 3.735-5.055 3.735-8.264s-1.43-6.11-3.736-8.264zm-.489 8.264c0-2.084-.915-3.967-2.384-5.391l-1.503 1.503c1.011 1.049 1.637 2.401 1.637 3.888 0 1.488-.623 2.841-1.634 3.891l1.503 1.503c1.468-1.424 2.381-3.309 2.381-5.394z"/></svg></button>`).append(`<button class='stop stop-${i}' >Parar Escuchando</button>`).append('<p class="description">'+w.Descripcion+"</p>").append('<h2 class="translate-title">English Translation</h2>').append('<p class="description">'+w.Description+"</p>"),$("#contents").append(N)}for(e.forEach(((e,a)=>{$(`.stop-${a}`).hide(),$(`.listen-${a}`).on("click",(function(){t.text=e,t.lang="es",console.log(a,t.text),speechSynthesis.speak(t),$(`.stop-${a}`).show()})),$(`.stop-${a}`).on("click",(function(){$(`.stop-${a}`).hide(),speechSynthesis.cancel()}))})),void 0,f=void 0,void 0,v=void 0,u=$(".leaflet-control-attribution")[0].innerHTML,f='View <a href="'+(o||"./csv/Chapters.csv")+'" target="_blank">data</a>',g=n("_authorName"),v=n("_authorURL"),g&&v?(v.indexOf("@")>0&&(v="mailto:"+v),f+=' by <a href="'+v+'">'+g+"</a> | "):f+=g?" by "+g+" | ":" | ",f+='View <a href="'+n("_githubRepo")+'">code</a>',n("_codeCredit")&&(f+=" by "+n("_codeCredit")),f+=" with ",$(".leaflet-control-attribution")[0].innerHTML=f+u,imgContainerHeight=parseInt(n("_imgContainerHeight")),imgContainerHeight>0&&$(".img-container").css({height:imgContainerHeight+"px","max-height":imgContainerHeight+"px"}),b[0]=-100,i=1;i<l.length;i++)b[i]=b[i-1]+$("div#container"+(i-1)).height()+70;b.push(Number.MAX_VALUE),$("div#contents").scroll((function(){var e=$(this).scrollTop();e<200&&$("#title").css("opacity",1-Math.min(1,e/100));for(var t=0;t<b.length-1;t++)if(e>=b[t]&&e<b[t+1]-140&&c!=t){location.hash=t+1,$(".chapter-container").removeClass("in-focus"),$("div#container"+t).addClass("in-focus").removeClass("out-focus"),y(c=t),map.hasLayer(d)&&map.removeLayer(d),map.hasLayer(h)&&map.removeLayer(h);var a=l[t];if(a.Overlay){var i=parseFloat(a["Overlay Transparency"])||1,o=a.Overlay;"geojson"===o.split(".").pop()?$.getJSON(o,(function(e){d=L.geoJson(e,{style:function(e){return{fillColor:e.properties.fillColor||"#ffffff",weight:e.properties.weight||1,opacity:e.properties.opacity||i,color:e.properties.color||"#cccccc",fillOpacity:e.properties.fillOpacity||.5}}}).addTo(map)})):d=L.tileLayer(a.Overlay,{opacity:i}).addTo(map)}if(a["GeoJSON Overlay"]&&$.getJSON(a["GeoJSON Overlay"],(function(e){var t={};if(a["GeoJSON Feature Properties"]){var i=a["GeoJSON Feature Properties"].split(";");for(var o in t={},i)2===i[o].split(":").length&&(t[i[o].split(":")[0].trim()]=i[o].split(":")[1].trim())}h=L.geoJson(e,{style:function(e){return{fillColor:e.properties.fillColor||t.fillColor||"#ffffff",weight:e.properties.weight||t.weight||1,opacity:e.properties.opacity||t.opacity||.5,color:e.properties.color||t.color||"#cccccc",fillOpacity:e.properties.fillOpacity||t.fillOpacity||.5}}}).addTo(map)})),a.Latitude&&a.Longitude){var r=a.Zoom?a.Zoom:15;map.flyTo([a.Latitude,a.Longitude],r,{animate:!0,duration:2})}break}})),$("#contents").append("       <div id='space-at-the-bottom'>         <a href='#top'>            <i class='fa fa-chevron-up'></i></br>           <small>Top</small>          </a>       </div>     "),$("<style>").prop("type","text/css").html("      #narration, #title {        background-color: "+s("_narrativeBackground","white")+";         color: "+s("_narrativeText","black")+";       }      a, a:visited, a:hover {        color: "+s("_narrativeLink","blue")+"       }      .in-focus {        background-color: "+s("_narrativeActive","#f0f0f0")+"       }").appendTo("head"),endPixels=parseInt(n("_pixelsAfterFinalChapter")),endPixels>100&&$("#space-at-the-bottom").css({height:endPixels/2+"px","padding-top":endPixels/2+"px"});var U=[];for(i in m)m[i]&&(m[i].addTo(map),m[i]._pixelsAbove=b[i],m[i].on("click",(function(){var e=parseInt($(this)[0]._pixelsAbove)+5;$("div#contents").animate({scrollTop:e+"px"})})),U.push(m[i].getLatLng()));if(map.fitBounds(U),$("#map, #narration, #title").css("visibility","visible"),$("div.loader").css("visibility","hidden"),$("div#container0").addClass("in-focus"),$("div#contents").animate({scrollTop:"1px"}),parseInt(location.hash.substr(1))){var R=parseInt(location.hash.substr(1))-1;$("#contents").animate({scrollTop:$("#container"+R).offset().top},2e3)}var V=n("_googleAnalytics");if(V&&V.length>=10){var Y=document.createElement("script");function Z(){dataLayer.push(arguments)}Y.setAttribute("src","https://www.googletagmanager.com/gtag/js?id="+V),document.head.appendChild(Y),window.dataLayer=window.dataLayer||[],Z("js",new Date),Z("config",V)}}$.get("csv/Options.csv",(function(e){$.get("csv/Chapters.csv",(function(t){l($.csv.toObjects(e),$.csv.toObjects(t))})).fail((function(e){alert("Found Options.csv, but could not read Chapters.csv")}))})).fail((function(e){var t=function(e){return Papa.parse(Papa.unparse(e[0].values),{header:!0}).data};if(o)if(r){var a="https://sheets.googleapis.com/v4/spreadsheets/",i=o.split("/d/")[1].split("/")[0];$.when($.getJSON(a+i+"/values/Options?key="+r),$.getJSON(a+i+"/values/Chapters?key="+r)).then((function(e,a){l(t(e),t(a))}))}else alert("You load data from a Google Sheet, you need to add a free Google API key");else alert("You need to specify a valid Google Sheet (googleDocURL)")}))}));

