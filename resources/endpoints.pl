endpoint(home, [currency, product_catalog, cart]).
endpoint(product, [product_catalog, currency, ad, cart, recommendation]).
%endpoint(listRecommendation, [product_catalog]).
endpoint(addToCart, [product_catalog, cart]).
endpoint(emptyCart, [cart]).
endpoint(viewCart, [cart, currency, product_catalog, shipping, recommendation]).
endpoint(placeOrder, [checkout, recommendation, product_catalog, currency, cart, shipping, payment, email]).
%endpoint(chPlaceOrder, [cart, product_catalog, currency, shipping, payment, email]).

