# fp-instagramscrapper
Instagram Scrapper for FreePascal

The demo shows an example of getting the values you need https://github.com/Al-Muhandis/fp-instagramscrapper/tree/master/demo

With the `TInstagramParser` class, you can get a bunch of other properties, media content, comments, stories, highlights etc.

``` Pascal
uses
  ... ..., 
  fphttpclientbroker;
  
// ... ... ...

```
In case of using the _synapse_ network components You should not forget to add in the dependency the `laz_synapse` package and `ssl_openssl` unit from the same package.
In case of using the _Indy 10_ network components You should not forget to add in the dependency the `indylaz` package and use common recommendation for Indy 10 in FPC.

Except for the need to use a proxy, it is recommended to use the native HTTP client class. At the moment synapse works perfectly with proxy in Windows, Indy 10 works in both Windows and Linux, but it is very complex and with a lot of its nuances.

Suggestions and improvements are welcome.