// float-controls.lisp
//
// This file is part of the cl-closure-template library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * EObject
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

function EObject (node) {
    if (node) {
        this.node = node;
    }
}

EObject.prototype.modelData = function () {
    var data = $.evalJSON(this.node.attr("json"))
    data.json = $.toJSON(data);
    return data;
};

EObject.prototype.toHTML = function (data) {
    throw "Method toHTML not implemented";
};

EObject.prototype.editForm = function (data) {
    throw "Method editForm not implemented";
};

EObject.prototype.replaceHTML = function (html) {
    this.node.after(html);
    this.node = this.node.next();
    this.node.prev().remove();
};

EObject.prototype.startEdit = function () {
    this.replaceHTML(this.editForm(this.modelData()));

    var obj = this;

    $('.cancel:first', this.node).click(function (evt) { obj.endEdit(); });

    this.node.ajaxForm({
        dataType: 'json', 
        success: function (data) { obj.endEdit(data)},
        error: function () { alert("Не удалось сохранить данные"); obj.endEdit() }
    });
};

EObject.prototype.endEdit = function (data) {
    this.replaceHTML(this.toHTML(data || this.modelData()));
    this.constructor(this.node);
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * EditableText
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

function EditableText (node) {
    if (node) {
        EObject.prototype.constructor.call(this, node);
        var obj = this;
        this.node.click(function (evt) { obj.startEdit(); });
    }
}

EditableText.prototype = new EObject;

EditableText.prototype.constructor = EditableText;

EditableText.prototype.toHTML = example.floatControls.view.editableText;

EditableText.prototype.editForm = example.floatControls.view.editText;
    
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * init
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

$(document).ready(function () {
   $(".editable-text").each( function (i, node) { new EditableText($(node)); });

   $(".panel").draggable().resizable();
});
