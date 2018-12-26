(function(){
    console.log('prepare for the best wishes for 2019');
    var app = Elm.Wish.init({
        node: document.getElementById('wish'),
        flags: {
            'width': 80,
            'height': 50,
            'x': 50,
            'y': 30,
            'delta': 3,
            'message': 'RGl0IGlzIG5vZyBlZW4gcHJvdG90eXBl'
        }
    });
})();
