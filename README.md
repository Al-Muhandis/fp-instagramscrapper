# fp-instagramscrapper
Instagram Scrapper for FreePascal

The demo shows an example of getting the values you need https://github.com/Al-Muhandis/fp-instagramscrapper/tree/master/demo

With the `TInstagramParser` class, you can get a bunch of other properties, media content, comments, stories, highlights etc.

You must add `fphttpclientbroker` unit to `uses` block and add TbFPHTTPClient begore `TInstagramParser` instance creation. For example, in `initialization` section

``` Pascal
uses
  ... ..., 
  fphttpclientbroker;
  
// ... ... ...

initialization
  TbFPHTTPClient.RegisterClientClass; // Native FCL HTTP Client
```

Suggestions and improvements are welcome.