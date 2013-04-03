(function( $ ){

    function init(element) {
        $(element).addClass("hidden");
        $(element).before("<a href='#' style='font-size:4pt; background: #eee; border: 1px solid #ccc; padding: 0px 3px; position:relative; top: -10px;'>O O O</a>");
        $(element).prev().click(function () {
            $(this).next().removeClass("hidden");
            $(this).remove();
        });
    }

    var methods = {
        init : function() {
            return this.each(function(i){
                init($(this));
            });
         },
         show : function() {
             return this.each(function(){
             });
         }
    };

    $.fn.hidden_bydef = function( method ) {
        if ( methods[method] ) {
            return methods[method].apply( this, Array.prototype.slice.call( arguments, 1 ));
        } else if ( typeof method === 'object' || ! method ) {
            return methods.init.apply( this, arguments );
        } else {
            $.error('Method ' +  method + ' does not exist on jQuery.hidden_bydef');
        }
    };

})(jQuery);
