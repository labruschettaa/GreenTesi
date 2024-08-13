
# Esempi di configurazioni considerate.
### **Configurazioni di CPU in server specifici**:

- **Dell PowerEdge T40 (Entry-level)**: Up to 4 cores.
- **Dell PowerEdge R540 (Mid-range)**: Up to 40 cores.
- **Dell PowerEdge R740 (High-end)**: Up to 56 cores.
- **Dell PowerEdge R940 (High-performance)**: Up to 112 cores.
---

### **Configurazioni di RAM per server specifici**:

- **Dell PowerEdge T40 (Entry-level)**: 8 GB - 32 GB DDR4.
- **Dell PowerEdge R540 (Mid-range)**: 32 GB - 256 GB DDR4.
- **Dell PowerEdge R740 (High-end)**: 128 GB - 3 TB DDR4.
- **Dell PowerEdge R940 (High-performance)**: 512 GB - 12 TB DDR4.
---

### **Configurazioni di banda per server specifici**:
- **Dell PowerEdge T40 (Entry-level)**: Up to 1 Gbps - 1 Gbps.
- **Dell PowerEdge R540 (Mid-range)**: Up to 10 Gbps - 10 Gbps.
- **Dell PowerEdge R740 (High-end)**: Up to 25 Gbps - 25 Gbps.
- **Dell PowerEdge R940 (High-performance)**: Up to 100 Gbps - 100 Gbps.
---
<br />

# OnlineBoutique

### 1. HomeHandler

---

- L'endpoint <b>Home</b> interagisce con i microservizi <b>Currency</b>, <b>ProductCatalog</b> e <b>Cart</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b>.

---

_handerls.go_, riga 118: \
`currencies, err := fe.getCurrencies(r.Context())`. \
_rpc.go_, riga 37: \
`currs, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).GetSupportedCurrencies(newCtx, &pb.Empty{})` \
Che interagisce con il microservizio <b>Currency</b>.

_handlers.go_, riga 123: \
`products, err := fe.getProducts(r.Context())` \
_rpc.go_, riga 61: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).ListProducts(newCtx, &pb.Empty{})` \
Che interagisce con il microservizio <b>ProductCatalog</b>.

_handlers.go_, riga 128: \
`cart, err := fe.getCart(r.Context(), sessionID(r))` \
_rpc.go_, riga 91: \
`resp, err := pb.NewCartServiceClient(fe.cartSvcConn).GetCart(newCtx, &pb.GetCartRequest{UserId: userID})` \
Che interagisce con il microservizio <b>Cart</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_CartService.cs_, riga 94: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`return _cartStore.GetCartAsync(request.UserId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartService.cs_, riga 90: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`var value = await _cache.GetAsync(userId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>.


### 2. ProductHandler

---

- L'endpoint <b>Product</b> interagisce con i microservizi <b>ProductCatalog</b>, <b>Currency</b> (x2), <b>Ad</b>, <b>Cart</b> e <b>Recommendation</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b>.
- Il microservizio <b>Recommendation</b> interagisce con il microservizio <b>ProductCatalog</b> (XN).
---

_handlers.go_, riga 217: \
`p, err := fe.getProduct(r.Context(), id)` \
_rpc.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b>.

_handlers.go_, riga 222: \
`currencies, err := fe.getCurrencies(r.Context())` \
_rpc.go_, riga 37: \
`currs, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).GetSupportedCurrencies(newCtx, &pb.Empty{})` \
Che interagisce con il microservizio <b>Currency</b>.

_handlers.go_, riga 228: \
`cart, err := fe.getCart(r.Context(), sessionID(r))` \
_rpc.go_, riga 91: \
`resp, err := pb.NewCartServiceClient(fe.cartSvcConn).GetCart(newCtx, &pb.GetCartRequest{UserId: userID})` \
Che interagisce con il microservizio <b>Cart</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_CartService.cs_, riga 94: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`return _cartStore.GetCartAsync(request.UserId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartService.cs_, riga 90: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`var value = await _cache.GetAsync(userId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>

_handlers.go_, riga 234: \
`price, err := fe.convertCurrency(r.Context(), p.GetPriceUsd(), currentCurrency(r))` \
_rpc.go_, riga 142: \
`res, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).Convert(newCtx, &pb.CurrencyConversionRequest` \
Che interagisce con il microservizio <b>Currency</b>.

_handlers.go_, riga 241: \
`recommendations, err := fe.getRecommendations(r.Context(), sessionID(r), []string{id})`\
_rpc.go_, riga 179: \
`resp, err := pb.NewRecommendationServiceClient(fe.recommendationSvcConn).ListRecommendations(newCtx, ` \
Che interagisce con il microservizio <b>Recommendation</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_recommendation_server.py_, riga 96: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cat_response = product_catalog_stub.ListProducts(demo_pb2.Empty(), timeout=1, metadata=metadata)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>ProductCatalog</b>.
_rpg.go_, riga 190: \
`p, err := fe.getProduct(newCtx, v)` \
_rpg.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b> N volte.

_handlers.go_, riga 254: \
`"ad":                fe.chooseAd(r.Context(), p.Categories, log),` \
_handlers.go_, riga 500: \
`ads, err := fe.getAd(ctx, ctxKeys)` \
_rpc.go_, riga 208: \
`resp, err := pb.NewAdServiceClient(fe.adSvcConn).GetAds(newCtx, &pb.AdRequest{` \
Che interagisce con il microservizio <b>Ad</b>.

### 3. AddToCart

---

- L'endpoint <b>AddToCart</b> interagisce con i microservizi <b>ProductCatalog</b>, <b>Cart</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b> (X2).

---

_handlers.go_, riga 280: \
`p, err := fe.getProduct(r.Context(), productID)` \
_rpc.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b>.

_handlers.go_, riga 286: \
`if err := fe.insertCart(r.Context(), sessionID(r), p.GetId(), int32(quantity)); err != nil {` \
_rpc.go_, riga 119: \
`_, err := pb.NewCartServiceClient(fe.cartSvcConn).AddItem(newCtx, &pb.AddItemRequest{` \
Che interagisce con il microservizio <b>Cart</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartStore,cs_, riga 40: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`var value = await _cache.GetAsync(userId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartStore_, riga 60: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`await _cache.SetAsync(userId, cart.ToByteArray());` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>. 

### 4. EmptyCart

---

- L'endpoint <b>EmptyCart</b> interagisce con il microservizio <b>Cart</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b>.

---

_handlers.go_, riga 298: \
`if err := fe.emptyCart(r.Context(), sessionID(r)); err != nil {` \
_rpc.go_, riga 105: \
`_, err := pb.NewCartServiceClient(fe.cartSvcConn).EmptyCart(newCtx, &pb.EmptyCartRequest{UserId: userID})` \
Che interagisce con il microservizio <b>Cart</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartStore_, riga 75: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`await _cache.SetAsync(userId, cart.ToByteArray());` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>. 

### 5. ViewCartHandler

---

- L'endpoint <b>ViewCart</b> interagisce con i microservizi <b>Cart</b>, <b>Currency</b> (XN+2), <b>ProductCatalog</b> (XN+1), <b>Shipping</b> e <b>Recommendation</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b>.
- Il microservizio <b>Recommendation</b> interagisce con il microservizio <b>ProductCatalog</b> (xN).

---

_handlers.go_, riga 309: \
`currencies, err := fe.getCurrencies(r.Context())` \
_rpc.go_, riga 37: \
`currs, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).GetSupportedCurrencies(newCtx, &pb.Empty{})` \
Che interagisce con il microservizio <b>Currency</b>. 

_handlers.go_, riga 314: \
`cart, err := fe.getCart(r.Context(), sessionID(r))` \
_rpc.go_, riga 91: \
`resp, err := pb.NewCartServiceClient(fe.cartSvcConn).GetCart(newCtx, &pb.GetCartRequest{UserId: userID})` \
Che interagisce con il microservizio <b>Cart</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_CartService.cs_, riga 94: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`return _cartStore.GetCartAsync(request.UserId);` \ 
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartService.cs_, riga 90: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`var value = await _cache.GetAsync(userId);` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>

_handlers.go_, riga 321: \
`recommendations, err := fe.getRecommendations(r.Context(), sessionID(r), cartIDs(cart))` \
_rpc.go_, riga 179: \
`resp, err := pb.NewRecommendationServiceClient(fe.recommendationSvcConn).ListRecommendations(newCtx, ` \
Che interagisce con il microservizio <b>Recommendation</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_recommendation_server.py_, riga 96: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cat_response = product_catalog_stub.ListProducts(demo_pb2.Empty(), timeout=1, metadata=metadata)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>ProductCatalog</b>. \
_rpg.go_, riga 190: \
`p, err := fe.getProduct(newCtx, v)` \
_rpg.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b> N volte.

_handlers.go_, riga 326: \
`shippingCost, err := fe.getShippingQuote(r.Context(), cart, currentCurrency(r))` \
_rpc.go_, riga 159: \
`quote, err := pb.NewShippingServiceClient(fe.shippingSvcConn).GetQuote(newCtx,` \
Che interagisce con il microservizio <b>Shipping</b>. \
_rpc.go_, riga 169: \
`localized, err := fe.convertCurrency(ctx, quote.GetCostUsd(), currency)` \
_rpc.go_, riga 142: \
`res, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).Convert(newCtx, &pb.CurrencyConversionRequest` \
Che interagisce con il microservizio <b>Currency</b>. 

_handlers.go_, riga 340: \
`p, err := fe.getProduct(r.Context(), item.GetProductId())` \
_rpc.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b> N volte. 

_handlers.go_, riga 345: \
`price, err := fe.convertCurrency(r.Context(), p.GetPriceUsd(), currentCurrency(r))` \
_rpc.go_, riga 142: \
`res, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).Convert(newCtx, &pb.CurrencyConversionRequest` \
Che interagisce con il microservizio <b>Currency</b> N volte. 

### 6. PlaceOrder

---

- L'endpoint <b>PlaceOrder</b> interagisce con i microservizi <b>Checkout</b>, <b>Recommendation</b>, <b>ProductCatalog</b> (XN) e <b>Currency</b>.
- Il microservizio <b>Checkout</b> interagisce con i microservizi <b>Cart</b> (X2), <b>ProductCatalog</b>, <b>Currency</b> (X2), <b>Shipping</b> (X2), <b>Payment</b>  e <b>Email</b>.
- Il microservizio <b>Cart</b> interagisce con la cache <b>Redis Cache</b>.
- Il microservizio <b>Recommendation</b> interagisce con il microservizio <b>ProductCatalog</b>.
---

_handlers.go_, riga 404: \
`order, err := pb.NewCheckoutServiceClient(fe.checkoutSvcConn).PlaceOrder(newCtx, &pb.PlaceOrderRequest{` \
Che interagisce con il microservizio <b>Checkout</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 313: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`prep, err := cs.prepareOrderItemsAndShippingQuoteFromCart(ctx, req.UserId, req.UserCurrency, req.Address)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 369: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cartItems, err := cs.getUserCart(ctx, userID)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 418: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cart, err := pb.NewCartServiceClient(cs.cartSvcConn).GetCart(newCtx, &pb.GetCartRequest{UserId: userID})` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Cart</b>. \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_CartService.cs_, riga 94: \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`return _cartStore.GetCartAsync(request.UserId);` \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartService.cs_, riga 90: \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`var value = await _cache.GetAsync(userId);` \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b> \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 373: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`orderItems, err := cs.prepOrderItems(ctx, cartItems, userCurrency)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 449: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cl := pb.NewProductCatalogServiceClient(cs.productCatalogSvcConn)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>ProductCatalog</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 470: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`price, err := cs.convertCurrency(secondCtx, product.GetPriceUsd(), userCurrency)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 491: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`result, err := pb.NewCurrencyServiceClient(cs.currencySvcConn).Convert(newCtx, &pb.CurrencyConversionRequest{`\
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Currency</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 377: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`shippingUSD, err := cs.quoteShipping(ctx, address, cartItems)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 398: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`shippingQuote, err := pb.NewShippingServiceClient(cs.shippingSvcConn).GetQuote(newCtx, &pb.GetQuoteRequest{` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Shipping</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 381: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`shippingPrice, err := cs.convertCurrency(ctx, shippingUSD, userCurrency)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 491: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`result, err := pb.NewCurrencyServiceClient(cs.currencySvcConn).Convert(newCtx, &pb.CurrencyConversionRequest{`\
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Currency</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 328: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`txID, err := cs.chargeCard(ctx, &total, req.CreditCard)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 510: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`paymentResp, err := pb.NewPaymentServiceClient(cs.paymentSvcConn).Charge(newCtx, &pb.ChargeRequest{` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Payment</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 334: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`shippingTrackingID, err := cs.shipOrder(ctx, req.Address, prep.cartItems)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 544: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`resp, err := pb.NewShippingServiceClient(cs.shippingSvcConn).ShipOrder(newCtx, &pb.ShipOrderRequest{` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Shipping</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 339: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`_ = cs.emptyUserCart(ctx, req.UserId)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 435: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`_, err := pb.NewCartServiceClient(cs.cartSvcConn).EmptyCart(newCtx, &pb.EmptyCartRequest{UserId: userID})` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Cart</b>. \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_RedisCartStore.cs_, riga 75: \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`await _cache.SetAsync(userId, cart.ToByteArray());` \
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con la cache <b>Redis Cache</b>.\
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 349: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`if err := cs.sendOrderConfirmation(ctx, req.Email, orderResult); err != nil {` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_main.go_, riga 529: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`_, err := pb.NewEmailServiceClient(cs.emailSvcConn).SendOrderConfirmation(newCtx, &pb.SendOrderConfirmationRequest{` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>Email</b>. 

_handlers.go_, riga 378: \
`recommendations, _ := fe.getRecommendations(r.Context(), sessionID(r), nil)` \
_rpc.go_, riga 179: \
`resp, err := pb.NewRecommendationServiceClient(fe.recommendationSvcConn).ListRecommendations(newCtx, ` \
Che interagisce con il microservizio <b>Recommendation</b>. \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;_recommendation_server.py_, riga 96: \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`cat_response = product_catalog_stub.ListProducts(demo_pb2.Empty(), timeout=1, metadata=metadata)` \
|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Che interagisce con il microservizio <b>ProductCatalog</b>. \
_rpg.go_, riga 190: \
`p, err := fe.getProduct(newCtx, v)` \
_rpg.go_, riga 76: \
`resp, err := pb.NewProductCatalogServiceClient(fe.productCatalogSvcConn).GetProduct(newCtx, &pb.GetProductRequest{Id: id})` \
Che interagisce con il microservizio <b>ProductCatalog</b> N volte.

_handlers.go, riga 386: \
`currencies, err := fe.getCurrencies(r.Context())` \
_rpc.go_, riga 37: \
`currs, err := pb.NewCurrencyServiceClient(fe.currencySvcConn).GetSupportedCurrencies(newCtx, &pb.Empty{})` \
Che interagisce con il microservizio <b>Currency</b>.

---
Note:
RadisCart vs SpannerCart ??
