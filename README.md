# fp-instagramscrapper
Instagram Scrapper for FreePascal

The demo shows an example of getting the values you need https://github.com/Al-Muhandis/fp-instagramscrapper/tree/master/demo

With the `TInstagramParser` class, you can get a bunch of other properties, media content, comments, stories, highlights etc.

You must add `fphttpclientbroker` or `synapsehttpclientbroker` unit to `uses` block and add `TbFPHTTPClient.RegisterClientClass` or `TSynapseHTTPClient.RegisterClientClass` before `TInstagramParser` instance creation. 
For example, in `initialization` section

``` Pascal
uses
  ... ..., 
  fphttpclientbroker;
  
// ... ... ...

initialization
  TbFPHTTPClient.RegisterClientClass; // Native FCL HTTP Client    
```
In case of using the _synapse_ network components You should not forget to add in the dependency the `laz_synapse` package and `ssl_openssl` unit from the same package.

Suggestions and improvements are welcome.