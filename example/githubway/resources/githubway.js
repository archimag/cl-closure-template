// githubway.lisp
//
// This file is part of the cl-closure-template library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

// show message detail


function updateJSONFields (json) {
    delete json.json;

    var shortJSONData = { };

    for (var i in json) {
        if (json[i] && typeof json[i] == "object") {
            updateJSONFields(json[i]);
        }
        else {
            shortJSONData[i] = json[i];
        }
    }

    json.json = $.toJSON(shortJSONData);

    return json;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * PObject
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

function JObject (editor, node) {
    if (editor) {
        this.editor = editor;
        this.node = node;
    }
}

JObject.prototype._asJSON = function () {
    return $.evalJSON(this.node.attr("json"));
};


JObject.prototype.asJSON = function () {
    var data = this._asJSON();
    data.json = $.toJSON(data);
    return data;
};


JObject.prototype.rect = function () {
    throw "Method rect not implemeted";
};

JObject.prototype.rtree = function () {
    throw "Method rtree not implemented";
};

JObject.prototype.toSVG = function (data) {
    throw "Method svg not implemented";
};

JObject.prototype.cleanReferences = function () { };

JObject.prototype.registerReferences = function () { };

JObject.prototype.setJSON = function (json) {
    updateJSONFields(json);

    this.clearReferences();

    this.node.after(this.toSVG(json));
    this.node = this.node.next();
    this.node.prev().remove();

    this.registerReferences();
};
