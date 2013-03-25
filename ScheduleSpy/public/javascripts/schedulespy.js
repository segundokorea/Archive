// Behavior for the main ScheduleSpy form
$(document).ready( function() {
  $('form').each( function () {
    var states = [ 'undisturbed', 'processing', 'validated', 'rejected' ];

    $(this).find( 'input#spy_submit' ).each( function() {
      // $(this).attr( 'disabled', true );
      $(this).click( function( e ) {
        $('.field > input').each( function() {
          if ( $(this).hasClass( 'undisturbed' ) ) {
            $(this).val( '' );
          }
        });

        $('form').submit();
        return false;
      });
    });

    $(this).find( '.field input' ).each( function() {
      var title = $(this).attr( 'title' );
      if ( title !== undefined && title !== false && title !== '' ) {
        $(this).qtip({
          content : title,
          position : {
            corner : {
              target : 'rightMiddle',
              tooltip : 'leftMiddle'
            }
          },
          style : {
            name : 'cream',
            tip : 'leftMiddle',
            width : 300
          },
          show : {
            solo : true,
            when : {
              event : 'focus'
            }
          },
          hide : {
            when : {
              event : 'blur'
            }
          }
        });
      }
    });

    $(this).find( '.field' ).each( function() {
      var current_label = $(this).find( 'label' ).hide();
      var current_input = $(this).find( 'input' );
      var using_select  = false;
      if ( current_input.length == 0 ) {
        current_input = $(this).find( 'select' );
        using_select  = true;
      }

      if ( !using_select ) {
        var undisturbed = true;
        for ( state in states ) {
          if ( current_input.hasClass( state ) ) {
            undisturbed = false;
          }
        }

        if ( undisturbed ) {
          current_input.addClass( 'undisturbed' ).val( current_label.text() + "..." );
        }
      }


      current_input.focus( function() {
        current_input.removeClass(); // All of 'em
        var current_label_text = $(this).parent().find( 'label' ).text() + "...";

        if ( $(this).val() === current_label_text ) {
          $(this).val( '' );
          $(this).removeClass( 'undisturbed' );
        }
      });


      current_input.blur( function() {
        current_input.removeClass(); // All of 'em
        var current_label_text = $(this).parent().find( 'label' ).text() + "...";

        if ( $(this).val() === '' || $(this).val() === current_label_text ) {
          if ( !using_select ) {
            $(this).val( current_label_text );
            $(this).addClass( 'undisturbed' );
          }
        } else {
          $(this).addClass( 'rejected' );
          switch ( $(this).attr( 'id' ) ) {
            case 'spy_email':
              if ( RegExp( '.+?@uchicago\.edu$' ).test( $(this).val() ) ) {
                $(this).removeClass( 'rejected' );
                $(this).addClass( 'validated' );
              }
              break;
            case 'spy_department':
              console.log($(this));
              if ( RegExp( '^[a-z]{4}$', 'i' ).test( $(this).val() ) ) {
                $(this).val( $(this).val().toUpperCase() );
                $(this).removeClass( 'rejected' );
                $(this).addClass( 'validated' );
              }
              break;
            case 'spy_course':
              if ( RegExp( '^[0-9]{3,5}$' ).test( $(this).val() ) ) {
                switch ( $(this).val().length ) {
                  case 3:
                    $(this).val( $(this).val() + '0' );
                  case 4:
                    $(this).val( $(this).val() + '0' );
                }
                $(this).removeClass( 'rejected' );
                $(this).addClass( 'validated' );
              }
              break;
            case 'spy_section':
              if ( RegExp( '^[a-z0-9]{2}$', 'i' ).test( $(this).val() ) ) {
                switch ( $(this).val().length ) {
                  case 1:
                    $(this).val( '0' + $(this).val() );
                }
                $(this).removeClass( 'rejected' );
                $(this).addClass( 'validated' );
              }
              break;
          }
        }

        var fields_validated = true;
        $('.field input').each( function() {
          fields_validated = fields_validated && $(this).hasClass( 'validated' );
        });

        $('.field select').each( function() {
          fields_validated = fields_validated && $(this).hasClass( 'validated' );
        });

        // if ( fields_validated ) {
        //   $('input#spy_submit' ).removeAttr( 'disabled' );
        // } else {
        //   $('input#spy_submit').attr( 'disabled', true );
        // }
      });
    });
  });
});
