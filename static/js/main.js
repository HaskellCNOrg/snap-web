(function ($) {

$.hcn = $.hcn || {};

$.hcn.initReplyToReplyBtn = function () {

    /**
     * Click Handler for "Reply to Relpy".
     * ajax get the form for reply
     */
    $('a[id^=add-comment-]').click(function (x) {
        var target = $(x.currentTarget),
            replySection = target.parent().prev('.replyOfReply')
            form   = replySection.find('form');

        if (form.length > 0) {
            form.find('textarea').focus();
        } else {
            $.get(["/topic", target.attr("data-topic"), target.attr("data-reply"), "reply"].join("/"), 
                  function (res) {
                      replySection.append(res);
                  }
            );
        }
    });

    /**
     * Dynamicly bind submit to the "reply to reply" form.
     * Submit it in ajax way.
     */
    $('.topicMain').on("submit", 'form[id^=add-comment-form-]', function (event) {
        event.preventDefault();
        var t = $(event.currentTarget),
            parent = t.parent();

        $.post(t[0].action, t.serialize(), function (res) {
          t.remove();
          parent.append(res);
        })
    });

  }

})(jQuery)