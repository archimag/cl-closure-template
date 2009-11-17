// messages.lisp
//
// This file is part of the cl-closure-template library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>

// show message detail

function showDetailMsg () {
    $.getJSON($(this).attr("href"), function (obj) {
        return function (data) {
            obj.replaceWith(messages.showMessageDetail(data));
        }
    }($(this).parent()));
}

// new message

function cancelNewMsg() {
    $("#createmsg").show();
    $("form").remove();
}

function showNewMsg (data) {
    cancelNewMsg();
    $("#messages").prepend(messages.showMessage(data));
    $(".message:first .fakelink").click(showDetailMsg);
}

function errorNewMsg () {
    cancelNewMsg();
    alert("Add message failed  :(");
}

function showCreateForm () {
    if ($("form").length == 0) {
        $(this).hide();
        $(this).after(messages.createMessageForm());
        $("#cancel").click(cancelNewMsg);
        $("form").ajaxForm({dataType:  'json', success: showNewMsg, error: errorNewMsg});
    }
}

// init

$(document).ready(function () {
    $('.showmsg').click(showDetailMsg);
    $('#createmsg').click(showCreateForm);
});

