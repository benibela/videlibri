#!/bin/bash 
set -e

export apk=/tmpshared/android-release.apk
export changesDE=/tmp/de.txt
export changesEN=/tmp/en.txt

source amazon.conf

export api=https://developer.amazon.com/api/appstore/v1


access_token=$( curl -k -X POST -H 'Content-Type: application/x-www-form-urlencoded' -d "grant_type=client_credentials&client_id=$client_id&client_secret=$client_secret&scope=appstore::apps:readwrite' https://api.amazon.com/auth/O2/token | xidel - -e '?access_token' )

echo access_token:
echo $access_token

headers=(-H  "Authorization: Bearer $access_token"  -H  "accept: application/json")


editid=$(curl -X POST "${headers[@]}"  "$api/applications/$appid/edits" | xidel - -e '?id' )


apkid=$(xidel "${headers[@]}" "$api/applications/$appid/edits/$editid/apks" -e '?*?id') 

etag=$(xidel -X GET "${headers[@]}" "$api/applications/$appid/edits/$editid/apks/$apkid" -e '$headers[starts-with(., "ETAG")]!substring-after(., ":")!normalize-space(.)')

echo starting uploading 

curl -X PUT "${headers[@]}" -H "Content-Type: application/vnd.android.package-archive" -H "If-Match: $etag" "$api/applications/$appid/edits/$editid/apks/$apkid/replace" --data-binary @"$apk"

echo upload complete

echo submit

xidel --variable changesDE,changesEN,access_token,api,appid,editid -e '
  declare function local:request($x,$url,$data,$headers) {
    trace( $x || " " || $url, "request")[0],
    x:request({"method": $x, "url": $api || "/" || $url, "post": $data, "headers": (x"Authorization: Bearer {$access_token}", "accept: application/json", $headers) })
  };
  declare function local:request($url) { local:request("GET", $url, (),())};
  declare function local:get-etag($headers) { $headers[starts-with(., "ETAG")]!substring-after(., ":")!normalize-space(.) };
  
  let $changes := {"de-DE": $changesDE, "en-US": $changesEN}
  for $lang in map:keys($changes)
  let $url := x"applications/{$appid}/edits/{$editid}/listings/{$lang}"
  let $listing := local:request($url)
  let $etag := local:get-etag($listing?headers)
  let $json := map:put($listing?json, "recentChanges",  file:read-text($changes($lang)) )
  return local:request("PUT", $url, serialize-json($json), "If-Match: "||$etag)?raw,
  
  
  
  local:request("POST", x"applications/{$appid}/edits/{$editid}/validate", (), ()),
  
  let $etag := local:get-etag(local:request(x"applications/{$appid}/edits/{$editid}")?headers)
  return local:request("POST", x"applications/{$appid}/edits/{$editid}/commit", (), "If-Match: "||$etag),
  
  
  "complete"
'



#"$api/" -e '$json?' 
