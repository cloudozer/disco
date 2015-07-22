var websocket;
var s;

function showScreen(txt) {
    $('#output').prepend('<p>' + txt + '</p>');
};

$(function () {

    var wsHost = "ws://" + window.location.host + "/websocket";
    if (!("WebSocket" in window)) {
        alert('websockets are not supported');
    } else {
        websocket = new WebSocket(wsHost);
        showScreen('<span>Connecting to the demo-server... </span>');
        websocket.onopen = function (evt) {
            onOpen(evt)
        };
        websocket.onclose = function (evt) {
            onClose(evt)
        };
        websocket.onmessage = function (evt) {
            onMessage(evt)
        };
        websocket.onerror = function (evt) {
            onError(evt)
        };
    }

    s = new sigma('container');

});

function onOpen(evt) {
    showScreen('<span style="color: green;">CONNECTED </span>');
};
function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};
function onMessage(evt) {
    var data = JSON.parse(evt.data);

    if ('start' == data.type) {
        showScreen('<span>Started DRC session </span>');
        s.graph.clear();
        s.refresh();
    }
    else if ('stop' == data.type) {
        showScreen('<span>DRC session done. Memory: ' + data.memory + '</span>');
    }
    else if ('partition_started' == data.type) {

        s.graph.addNode({
            id: data.x + ", " + data.y,
            x: data.x,
            y: data.y,
            size: 2,
            color: '#A52A2A'
        });

        s.refresh();
    }
    else if ('stitcher_connected' == data.type) {

        var stitcher_x = data.stitcher.nb + (data.stitcher.ne - data.stitcher.nb) / 2;
        var stitcher_y = data.stitcher.mb + (data.stitcher.me - data.stitcher.mb) / 2;
        var stitcher_id = '(' + data.stitcher.nb + ',' + data.stitcher.ne + '),(' + data.stitcher.mb + ',' + data.stitcher.me + ')';

        var getSourceId = function (source) {
            if ('partition' == source.type)
                return data.source.x + ", " + data.source.y;
            else if ('stitcher' == source.type)
                return '(' + source.nb + ',' + source.ne + '),(' + source.mb + ',' + source.me + ')';
        };

        var source_id = getSourceId(data.source);

        if (!s.graph.nodes(stitcher_id))
            s.graph.addNode({
                id: stitcher_id,
                x: stitcher_x,
                y: stitcher_y,
                size: 3,
                color: '#1874CD'
            });

        s.graph.addEdge({
            id: source_id + '-' + stitcher_id,
            source: source_id,
            target: stitcher_id
        });

        s.refresh();
    }

};
function onError(evt) {
    showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
};
