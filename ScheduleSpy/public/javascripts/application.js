// ScheduleSpy
$(document).ready( function() {
  $('#wtf').qtip({
    content : $('#wtf').attr('data-description'),
    position : {
      corner : {
        target : 'bottomMiddle',
        tooltip : 'topMiddle'
      }
    },
    style : {
      name : 'cream',
      tip : 'topMiddle',
      width : 350
    },
    show : 'click',
    hide : 'unfocus'
  });
});
