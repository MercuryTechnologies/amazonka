{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFront.Types.DistributionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DistributionConfig where

import Amazonka.CloudFront.Types.Aliases
import Amazonka.CloudFront.Types.CacheBehaviors
import Amazonka.CloudFront.Types.CustomErrorResponses
import Amazonka.CloudFront.Types.DefaultCacheBehavior
import Amazonka.CloudFront.Types.HttpVersion
import Amazonka.CloudFront.Types.LoggingConfig
import Amazonka.CloudFront.Types.OriginGroups
import Amazonka.CloudFront.Types.Origins
import Amazonka.CloudFront.Types.PriceClass
import Amazonka.CloudFront.Types.Restrictions
import Amazonka.CloudFront.Types.ViewerCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A distribution configuration.
--
-- /See:/ 'newDistributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { -- | (Optional) Specify the maximum HTTP version that you want viewers to use
    -- to communicate with CloudFront. The default value for new web
    -- distributions is http2. Viewers that don\'t support HTTP\/2
    -- automatically use an earlier HTTP version.
    --
    -- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
    -- or later, and must support Server Name Identification (SNI).
    --
    -- In general, configuring CloudFront to communicate with viewers using
    -- HTTP\/2 reduces latency. You can improve performance by optimizing for
    -- HTTP\/2. For more information, do an Internet search for \"http\/2
    -- optimization.\"
    httpVersion :: Prelude.Maybe HttpVersion,
    -- | A complex type that contains information about origin groups for this
    -- distribution.
    originGroups :: Prelude.Maybe OriginGroups,
    -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this distribution.
    aliases :: Prelude.Maybe Aliases,
    -- | The object that you want CloudFront to request from your origin (for
    -- example, @index.html@) when a viewer requests the root URL for your
    -- distribution (@http:\/\/www.example.com@) instead of an object in your
    -- distribution (@http:\/\/www.example.com\/product-description.html@).
    -- Specifying a default root object avoids exposing the contents of your
    -- distribution.
    --
    -- Specify only the object name, for example, @index.html@. Don\'t add a
    -- @\/@ before the object name.
    --
    -- If you don\'t want to specify a default root object when you create a
    -- distribution, include an empty @DefaultRootObject@ element.
    --
    -- To delete the default root object from an existing distribution, update
    -- the distribution configuration and include an empty @DefaultRootObject@
    -- element.
    --
    -- To replace the default root object, update the distribution
    -- configuration and specify the new object.
    --
    -- For more information about the default root object, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
    -- in the /Amazon CloudFront Developer Guide/.
    defaultRootObject :: Prelude.Maybe Prelude.Text,
    -- | The price class that corresponds with the maximum price that you want to
    -- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
    -- responds to requests for your objects from all CloudFront edge
    -- locations.
    --
    -- If you specify a price class other than @PriceClass_All@, CloudFront
    -- serves your objects from the CloudFront edge location that has the
    -- lowest latency among the edge locations in your price class. Viewers who
    -- are in or near regions that are excluded from your specified price class
    -- may encounter slower performance.
    --
    -- For more information about price classes, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
    -- in the /Amazon CloudFront Developer Guide/. For information about
    -- CloudFront pricing, including how price classes (such as Price Class
    -- 100) map to CloudFront regions, see
    -- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
    priceClass :: Prelude.Maybe PriceClass,
    -- | A complex type that controls the following:
    --
    -- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
    --     range with custom error messages before returning the response to
    --     the viewer.
    --
    -- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
    --     range.
    --
    -- For more information about custom error pages, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
    -- in the /Amazon CloudFront Developer Guide/.
    customErrorResponses :: Prelude.Maybe CustomErrorResponses,
    -- | A unique identifier that specifies the WAF web ACL, if any, to associate
    -- with this distribution. To specify a web ACL created using the latest
    -- version of WAF, use the ACL ARN, for example
    -- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
    -- To specify a web ACL created using WAF Classic, use the ACL ID, for
    -- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
    --
    -- WAF is a web application firewall that lets you monitor the HTTP and
    -- HTTPS requests that are forwarded to CloudFront, and lets you control
    -- access to your content. Based on conditions that you specify, such as
    -- the IP addresses that requests originate from or the values of query
    -- strings, CloudFront responds to requests either with the requested
    -- content or with an HTTP 403 status code (Forbidden). You can also
    -- configure CloudFront to return a custom error page when a request is
    -- blocked. For more information about WAF, see the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
    webACLId :: Prelude.Maybe Prelude.Text,
    -- | A complex type that determines the distribution’s SSL\/TLS configuration
    -- for communicating with viewers.
    viewerCertificate :: Prelude.Maybe ViewerCertificate,
    -- | A complex type that identifies ways in which you want to restrict
    -- distribution of your content.
    restrictions :: Prelude.Maybe Restrictions,
    -- | A complex type that controls whether access logs are written for the
    -- distribution.
    --
    -- For more information about logging, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
    -- in the /Amazon CloudFront Developer Guide/.
    logging :: Prelude.Maybe LoggingConfig,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: Prelude.Maybe CacheBehaviors,
    -- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6
    -- address for your distribution, specify @true@. If you specify @false@,
    -- CloudFront responds to IPv6 DNS requests with the DNS response code
    -- @NOERROR@ and with no IP addresses. This allows viewers to submit a
    -- second request, for an IPv4 address for your distribution.
    --
    -- In general, you should enable IPv6 if you have users on IPv6 networks
    -- who want to access your content. However, if you\'re using signed URLs
    -- or signed cookies to restrict access to your content, and if you\'re
    -- using a custom policy that includes the @IpAddress@ parameter to
    -- restrict the IP addresses that can access your content, don\'t enable
    -- IPv6. If you want to restrict access to some content by IP address and
    -- not restrict access to other content (or restrict access but not by IP
    -- address), you can create two distributions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you\'re using an Route 53 Amazon Web Services Integration alias
    -- resource record set to route traffic to your CloudFront distribution,
    -- you need to create a second alias resource record set when both of the
    -- following are true:
    --
    -- -   You enable IPv6 for the distribution
    --
    -- -   You\'re using alternate domain names in the URLs for your objects
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
    -- in the /Route 53 Amazon Web Services Integration Developer Guide/.
    --
    -- If you created a CNAME resource record set, either with Route 53 Amazon
    -- Web Services Integration or with another DNS service, you don\'t need to
    -- make any changes. A CNAME record will route traffic to your distribution
    -- regardless of the IP address format of the viewer request.
    isIPV6Enabled :: Prelude.Maybe Prelude.Bool,
    -- | A unique value (for example, a date-time stamp) that ensures that the
    -- request can\'t be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of
    -- the @DistributionConfig@ object), CloudFront creates a new distribution.
    --
    -- If @CallerReference@ is a value that you already sent in a previous
    -- request to create a distribution, CloudFront returns a
    -- @DistributionAlreadyExists@ error.
    callerReference :: Prelude.Text,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Origins,
    -- | A complex type that describes the default cache behavior if you don\'t
    -- specify a @CacheBehavior@ element or if files don\'t match any of the
    -- values of @PathPattern@ in @CacheBehavior@ elements. You must create
    -- exactly one default cache behavior.
    defaultCacheBehavior :: DefaultCacheBehavior,
    -- | An optional comment to describe the distribution. The comment cannot be
    -- longer than 128 characters.
    comment :: Core.Sensitive Prelude.Text,
    -- | From this field, you can enable or disable the selected distribution.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpVersion', 'distributionConfig_httpVersion' - (Optional) Specify the maximum HTTP version that you want viewers to use
-- to communicate with CloudFront. The default value for new web
-- distributions is http2. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
-- or later, and must support Server Name Identification (SNI).
--
-- In general, configuring CloudFront to communicate with viewers using
-- HTTP\/2 reduces latency. You can improve performance by optimizing for
-- HTTP\/2. For more information, do an Internet search for \"http\/2
-- optimization.\"
--
-- 'originGroups', 'distributionConfig_originGroups' - A complex type that contains information about origin groups for this
-- distribution.
--
-- 'aliases', 'distributionConfig_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- 'defaultRootObject', 'distributionConfig_defaultRootObject' - The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@http:\/\/www.example.com@) instead of an object in your
-- distribution (@http:\/\/www.example.com\/product-description.html@).
-- Specifying a default root object avoids exposing the contents of your
-- distribution.
--
-- Specify only the object name, for example, @index.html@. Don\'t add a
-- @\/@ before the object name.
--
-- If you don\'t want to specify a default root object when you create a
-- distribution, include an empty @DefaultRootObject@ element.
--
-- To delete the default root object from an existing distribution, update
-- the distribution configuration and include an empty @DefaultRootObject@
-- element.
--
-- To replace the default root object, update the distribution
-- configuration and specify the new object.
--
-- For more information about the default root object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'priceClass', 'distributionConfig_priceClass' - The price class that corresponds with the maximum price that you want to
-- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
-- responds to requests for your objects from all CloudFront edge
-- locations.
--
-- If you specify a price class other than @PriceClass_All@, CloudFront
-- serves your objects from the CloudFront edge location that has the
-- lowest latency among the edge locations in your price class. Viewers who
-- are in or near regions that are excluded from your specified price class
-- may encounter slower performance.
--
-- For more information about price classes, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
-- in the /Amazon CloudFront Developer Guide/. For information about
-- CloudFront pricing, including how price classes (such as Price Class
-- 100) map to CloudFront regions, see
-- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
--
-- 'customErrorResponses', 'distributionConfig_customErrorResponses' - A complex type that controls the following:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'webACLId', 'distributionConfig_webACLId' - A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution. To specify a web ACL created using the latest
-- version of WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
--
-- 'viewerCertificate', 'distributionConfig_viewerCertificate' - A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
--
-- 'restrictions', 'distributionConfig_restrictions' - A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- 'logging', 'distributionConfig_logging' - A complex type that controls whether access logs are written for the
-- distribution.
--
-- For more information about logging, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'cacheBehaviors', 'distributionConfig_cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- 'isIPV6Enabled', 'distributionConfig_isIPV6Enabled' - If you want CloudFront to respond to IPv6 DNS requests with an IPv6
-- address for your distribution, specify @true@. If you specify @false@,
-- CloudFront responds to IPv6 DNS requests with the DNS response code
-- @NOERROR@ and with no IP addresses. This allows viewers to submit a
-- second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks
-- who want to access your content. However, if you\'re using signed URLs
-- or signed cookies to restrict access to your content, and if you\'re
-- using a custom policy that includes the @IpAddress@ parameter to
-- restrict the IP addresses that can access your content, don\'t enable
-- IPv6. If you want to restrict access to some content by IP address and
-- not restrict access to other content (or restrict access but not by IP
-- address), you can create two distributions. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you\'re using an Route 53 Amazon Web Services Integration alias
-- resource record set to route traffic to your CloudFront distribution,
-- you need to create a second alias resource record set when both of the
-- following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Route 53 Amazon Web Services Integration Developer Guide/.
--
-- If you created a CNAME resource record set, either with Route 53 Amazon
-- Web Services Integration or with another DNS service, you don\'t need to
-- make any changes. A CNAME record will route traffic to your distribution
-- regardless of the IP address format of the viewer request.
--
-- 'callerReference', 'distributionConfig_callerReference' - A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @DistributionConfig@ object), CloudFront creates a new distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
--
-- 'origins', 'distributionConfig_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'defaultCacheBehavior', 'distributionConfig_defaultCacheBehavior' - A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
--
-- 'comment', 'distributionConfig_comment' - An optional comment to describe the distribution. The comment cannot be
-- longer than 128 characters.
--
-- 'enabled', 'distributionConfig_enabled' - From this field, you can enable or disable the selected distribution.
newDistributionConfig ::
  -- | 'callerReference'
  Prelude.Text ->
  -- | 'origins'
  Origins ->
  -- | 'defaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'enabled'
  Prelude.Bool ->
  DistributionConfig
newDistributionConfig
  pCallerReference_
  pOrigins_
  pDefaultCacheBehavior_
  pComment_
  pEnabled_ =
    DistributionConfig'
      { httpVersion = Prelude.Nothing,
        originGroups = Prelude.Nothing,
        aliases = Prelude.Nothing,
        defaultRootObject = Prelude.Nothing,
        priceClass = Prelude.Nothing,
        customErrorResponses = Prelude.Nothing,
        webACLId = Prelude.Nothing,
        viewerCertificate = Prelude.Nothing,
        restrictions = Prelude.Nothing,
        logging = Prelude.Nothing,
        cacheBehaviors = Prelude.Nothing,
        isIPV6Enabled = Prelude.Nothing,
        callerReference = pCallerReference_,
        origins = pOrigins_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        comment = Core._Sensitive Lens.# pComment_,
        enabled = pEnabled_
      }

-- | (Optional) Specify the maximum HTTP version that you want viewers to use
-- to communicate with CloudFront. The default value for new web
-- distributions is http2. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
-- or later, and must support Server Name Identification (SNI).
--
-- In general, configuring CloudFront to communicate with viewers using
-- HTTP\/2 reduces latency. You can improve performance by optimizing for
-- HTTP\/2. For more information, do an Internet search for \"http\/2
-- optimization.\"
distributionConfig_httpVersion :: Lens.Lens' DistributionConfig (Prelude.Maybe HttpVersion)
distributionConfig_httpVersion = Lens.lens (\DistributionConfig' {httpVersion} -> httpVersion) (\s@DistributionConfig' {} a -> s {httpVersion = a} :: DistributionConfig)

-- | A complex type that contains information about origin groups for this
-- distribution.
distributionConfig_originGroups :: Lens.Lens' DistributionConfig (Prelude.Maybe OriginGroups)
distributionConfig_originGroups = Lens.lens (\DistributionConfig' {originGroups} -> originGroups) (\s@DistributionConfig' {} a -> s {originGroups = a} :: DistributionConfig)

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
distributionConfig_aliases :: Lens.Lens' DistributionConfig (Prelude.Maybe Aliases)
distributionConfig_aliases = Lens.lens (\DistributionConfig' {aliases} -> aliases) (\s@DistributionConfig' {} a -> s {aliases = a} :: DistributionConfig)

-- | The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@http:\/\/www.example.com@) instead of an object in your
-- distribution (@http:\/\/www.example.com\/product-description.html@).
-- Specifying a default root object avoids exposing the contents of your
-- distribution.
--
-- Specify only the object name, for example, @index.html@. Don\'t add a
-- @\/@ before the object name.
--
-- If you don\'t want to specify a default root object when you create a
-- distribution, include an empty @DefaultRootObject@ element.
--
-- To delete the default root object from an existing distribution, update
-- the distribution configuration and include an empty @DefaultRootObject@
-- element.
--
-- To replace the default root object, update the distribution
-- configuration and specify the new object.
--
-- For more information about the default root object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_defaultRootObject :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_defaultRootObject = Lens.lens (\DistributionConfig' {defaultRootObject} -> defaultRootObject) (\s@DistributionConfig' {} a -> s {defaultRootObject = a} :: DistributionConfig)

-- | The price class that corresponds with the maximum price that you want to
-- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
-- responds to requests for your objects from all CloudFront edge
-- locations.
--
-- If you specify a price class other than @PriceClass_All@, CloudFront
-- serves your objects from the CloudFront edge location that has the
-- lowest latency among the edge locations in your price class. Viewers who
-- are in or near regions that are excluded from your specified price class
-- may encounter slower performance.
--
-- For more information about price classes, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
-- in the /Amazon CloudFront Developer Guide/. For information about
-- CloudFront pricing, including how price classes (such as Price Class
-- 100) map to CloudFront regions, see
-- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
distributionConfig_priceClass :: Lens.Lens' DistributionConfig (Prelude.Maybe PriceClass)
distributionConfig_priceClass = Lens.lens (\DistributionConfig' {priceClass} -> priceClass) (\s@DistributionConfig' {} a -> s {priceClass = a} :: DistributionConfig)

-- | A complex type that controls the following:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_customErrorResponses :: Lens.Lens' DistributionConfig (Prelude.Maybe CustomErrorResponses)
distributionConfig_customErrorResponses = Lens.lens (\DistributionConfig' {customErrorResponses} -> customErrorResponses) (\s@DistributionConfig' {} a -> s {customErrorResponses = a} :: DistributionConfig)

-- | A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution. To specify a web ACL created using the latest
-- version of WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
distributionConfig_webACLId :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_webACLId = Lens.lens (\DistributionConfig' {webACLId} -> webACLId) (\s@DistributionConfig' {} a -> s {webACLId = a} :: DistributionConfig)

-- | A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
distributionConfig_viewerCertificate :: Lens.Lens' DistributionConfig (Prelude.Maybe ViewerCertificate)
distributionConfig_viewerCertificate = Lens.lens (\DistributionConfig' {viewerCertificate} -> viewerCertificate) (\s@DistributionConfig' {} a -> s {viewerCertificate = a} :: DistributionConfig)

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
distributionConfig_restrictions :: Lens.Lens' DistributionConfig (Prelude.Maybe Restrictions)
distributionConfig_restrictions = Lens.lens (\DistributionConfig' {restrictions} -> restrictions) (\s@DistributionConfig' {} a -> s {restrictions = a} :: DistributionConfig)

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- For more information about logging, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_logging :: Lens.Lens' DistributionConfig (Prelude.Maybe LoggingConfig)
distributionConfig_logging = Lens.lens (\DistributionConfig' {logging} -> logging) (\s@DistributionConfig' {} a -> s {logging = a} :: DistributionConfig)

-- | A complex type that contains zero or more @CacheBehavior@ elements.
distributionConfig_cacheBehaviors :: Lens.Lens' DistributionConfig (Prelude.Maybe CacheBehaviors)
distributionConfig_cacheBehaviors = Lens.lens (\DistributionConfig' {cacheBehaviors} -> cacheBehaviors) (\s@DistributionConfig' {} a -> s {cacheBehaviors = a} :: DistributionConfig)

-- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6
-- address for your distribution, specify @true@. If you specify @false@,
-- CloudFront responds to IPv6 DNS requests with the DNS response code
-- @NOERROR@ and with no IP addresses. This allows viewers to submit a
-- second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks
-- who want to access your content. However, if you\'re using signed URLs
-- or signed cookies to restrict access to your content, and if you\'re
-- using a custom policy that includes the @IpAddress@ parameter to
-- restrict the IP addresses that can access your content, don\'t enable
-- IPv6. If you want to restrict access to some content by IP address and
-- not restrict access to other content (or restrict access but not by IP
-- address), you can create two distributions. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you\'re using an Route 53 Amazon Web Services Integration alias
-- resource record set to route traffic to your CloudFront distribution,
-- you need to create a second alias resource record set when both of the
-- following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Route 53 Amazon Web Services Integration Developer Guide/.
--
-- If you created a CNAME resource record set, either with Route 53 Amazon
-- Web Services Integration or with another DNS service, you don\'t need to
-- make any changes. A CNAME record will route traffic to your distribution
-- regardless of the IP address format of the viewer request.
distributionConfig_isIPV6Enabled :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Bool)
distributionConfig_isIPV6Enabled = Lens.lens (\DistributionConfig' {isIPV6Enabled} -> isIPV6Enabled) (\s@DistributionConfig' {} a -> s {isIPV6Enabled = a} :: DistributionConfig)

-- | A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @DistributionConfig@ object), CloudFront creates a new distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
distributionConfig_callerReference :: Lens.Lens' DistributionConfig Prelude.Text
distributionConfig_callerReference = Lens.lens (\DistributionConfig' {callerReference} -> callerReference) (\s@DistributionConfig' {} a -> s {callerReference = a} :: DistributionConfig)

-- | A complex type that contains information about origins for this
-- distribution.
distributionConfig_origins :: Lens.Lens' DistributionConfig Origins
distributionConfig_origins = Lens.lens (\DistributionConfig' {origins} -> origins) (\s@DistributionConfig' {} a -> s {origins = a} :: DistributionConfig)

-- | A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
distributionConfig_defaultCacheBehavior :: Lens.Lens' DistributionConfig DefaultCacheBehavior
distributionConfig_defaultCacheBehavior = Lens.lens (\DistributionConfig' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@DistributionConfig' {} a -> s {defaultCacheBehavior = a} :: DistributionConfig)

-- | An optional comment to describe the distribution. The comment cannot be
-- longer than 128 characters.
distributionConfig_comment :: Lens.Lens' DistributionConfig Prelude.Text
distributionConfig_comment = Lens.lens (\DistributionConfig' {comment} -> comment) (\s@DistributionConfig' {} a -> s {comment = a} :: DistributionConfig) Prelude.. Core._Sensitive

-- | From this field, you can enable or disable the selected distribution.
distributionConfig_enabled :: Lens.Lens' DistributionConfig Prelude.Bool
distributionConfig_enabled = Lens.lens (\DistributionConfig' {enabled} -> enabled) (\s@DistributionConfig' {} a -> s {enabled = a} :: DistributionConfig)

instance Core.FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      Prelude.<$> (x Core..@? "HttpVersion")
      Prelude.<*> (x Core..@? "OriginGroups")
      Prelude.<*> (x Core..@? "Aliases")
      Prelude.<*> (x Core..@? "DefaultRootObject")
      Prelude.<*> (x Core..@? "PriceClass")
      Prelude.<*> (x Core..@? "CustomErrorResponses")
      Prelude.<*> (x Core..@? "WebACLId")
      Prelude.<*> (x Core..@? "ViewerCertificate")
      Prelude.<*> (x Core..@? "Restrictions")
      Prelude.<*> (x Core..@? "Logging")
      Prelude.<*> (x Core..@? "CacheBehaviors")
      Prelude.<*> (x Core..@? "IsIPV6Enabled")
      Prelude.<*> (x Core..@ "CallerReference")
      Prelude.<*> (x Core..@ "Origins")
      Prelude.<*> (x Core..@ "DefaultCacheBehavior")
      Prelude.<*> (x Core..@ "Comment")
      Prelude.<*> (x Core..@ "Enabled")

instance Prelude.Hashable DistributionConfig where
  hashWithSalt salt' DistributionConfig' {..} =
    salt' `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` defaultCacheBehavior
      `Prelude.hashWithSalt` origins
      `Prelude.hashWithSalt` callerReference
      `Prelude.hashWithSalt` isIPV6Enabled
      `Prelude.hashWithSalt` cacheBehaviors
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` viewerCertificate
      `Prelude.hashWithSalt` webACLId
      `Prelude.hashWithSalt` customErrorResponses
      `Prelude.hashWithSalt` priceClass
      `Prelude.hashWithSalt` defaultRootObject
      `Prelude.hashWithSalt` aliases
      `Prelude.hashWithSalt` originGroups
      `Prelude.hashWithSalt` httpVersion

instance Prelude.NFData DistributionConfig where
  rnf DistributionConfig' {..} =
    Prelude.rnf httpVersion
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf defaultCacheBehavior
      `Prelude.seq` Prelude.rnf origins
      `Prelude.seq` Prelude.rnf callerReference
      `Prelude.seq` Prelude.rnf isIPV6Enabled
      `Prelude.seq` Prelude.rnf cacheBehaviors
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf viewerCertificate
      `Prelude.seq` Prelude.rnf webACLId
      `Prelude.seq` Prelude.rnf customErrorResponses
      `Prelude.seq` Prelude.rnf priceClass
      `Prelude.seq` Prelude.rnf defaultRootObject
      `Prelude.seq` Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf originGroups

instance Core.ToXML DistributionConfig where
  toXML DistributionConfig' {..} =
    Prelude.mconcat
      [ "HttpVersion" Core.@= httpVersion,
        "OriginGroups" Core.@= originGroups,
        "Aliases" Core.@= aliases,
        "DefaultRootObject" Core.@= defaultRootObject,
        "PriceClass" Core.@= priceClass,
        "CustomErrorResponses" Core.@= customErrorResponses,
        "WebACLId" Core.@= webACLId,
        "ViewerCertificate" Core.@= viewerCertificate,
        "Restrictions" Core.@= restrictions,
        "Logging" Core.@= logging,
        "CacheBehaviors" Core.@= cacheBehaviors,
        "IsIPV6Enabled" Core.@= isIPV6Enabled,
        "CallerReference" Core.@= callerReference,
        "Origins" Core.@= origins,
        "DefaultCacheBehavior" Core.@= defaultCacheBehavior,
        "Comment" Core.@= comment,
        "Enabled" Core.@= enabled
      ]
