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
-- Module      : Amazonka.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LightsailDistribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.CacheBehavior
import Amazonka.Lightsail.Types.CacheBehaviorPerPath
import Amazonka.Lightsail.Types.CacheSettings
import Amazonka.Lightsail.Types.IpAddressType
import Amazonka.Lightsail.Types.Origin
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lightsail content delivery network (CDN)
-- distribution.
--
-- /See:/ 'newLightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { -- | The status of the distribution.
    status :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the origin resource of the distribution, such
    -- as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Prelude.Maybe Origin,
    -- | The name of the SSL\/TLS certificate attached to the distribution, if
    -- any.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @Distribution@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the distribution.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the distribution was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | An object that describes the location of the distribution, such as the
    -- AWS Region and Availability Zone.
    --
    -- Lightsail distributions are global resources that can reference an
    -- origin in any AWS Region, and distribute its content globally. However,
    -- all distributions are located in the @us-east-1@ Region.
    location :: Prelude.Maybe ResourceLocation,
    -- | An object that describes the cache behavior settings of the
    -- distribution.
    cacheBehaviorSettings :: Prelude.Maybe CacheSettings,
    -- | The alternate domain names of the distribution.
    alternativeDomainNames :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the bundle currently applied to the distribution.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the bundle that is currently applied to your
    -- distribution, specified using the @distributionName@ parameter, can be
    -- changed to another bundle.
    --
    -- Use the @UpdateDistributionBundle@ action to change your distribution\'s
    -- bundle.
    ableToUpdateBundle :: Prelude.Maybe Prelude.Bool,
    -- | The public DNS of the origin.
    originPublicDNS :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the distribution.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IP address type of the distribution.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | Indicates whether the distribution is enabled.
    isEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail distribution. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the default cache behavior of the distribution.
    defaultCacheBehavior :: Prelude.Maybe CacheBehavior,
    -- | An array of objects that describe the per-path cache behavior of the
    -- distribution.
    cacheBehaviors :: Prelude.Maybe [CacheBehaviorPerPath],
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LightsailDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'lightsailDistribution_status' - The status of the distribution.
--
-- 'origin', 'lightsailDistribution_origin' - An object that describes the origin resource of the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- 'certificateName', 'lightsailDistribution_certificateName' - The name of the SSL\/TLS certificate attached to the distribution, if
-- any.
--
-- 'resourceType', 'lightsailDistribution_resourceType' - The Lightsail resource type (e.g., @Distribution@).
--
-- 'arn', 'lightsailDistribution_arn' - The Amazon Resource Name (ARN) of the distribution.
--
-- 'createdAt', 'lightsailDistribution_createdAt' - The timestamp when the distribution was created.
--
-- 'location', 'lightsailDistribution_location' - An object that describes the location of the distribution, such as the
-- AWS Region and Availability Zone.
--
-- Lightsail distributions are global resources that can reference an
-- origin in any AWS Region, and distribute its content globally. However,
-- all distributions are located in the @us-east-1@ Region.
--
-- 'cacheBehaviorSettings', 'lightsailDistribution_cacheBehaviorSettings' - An object that describes the cache behavior settings of the
-- distribution.
--
-- 'alternativeDomainNames', 'lightsailDistribution_alternativeDomainNames' - The alternate domain names of the distribution.
--
-- 'bundleId', 'lightsailDistribution_bundleId' - The ID of the bundle currently applied to the distribution.
--
-- 'ableToUpdateBundle', 'lightsailDistribution_ableToUpdateBundle' - Indicates whether the bundle that is currently applied to your
-- distribution, specified using the @distributionName@ parameter, can be
-- changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution\'s
-- bundle.
--
-- 'originPublicDNS', 'lightsailDistribution_originPublicDNS' - The public DNS of the origin.
--
-- 'domainName', 'lightsailDistribution_domainName' - The domain name of the distribution.
--
-- 'name', 'lightsailDistribution_name' - The name of the distribution.
--
-- 'ipAddressType', 'lightsailDistribution_ipAddressType' - The IP address type of the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'isEnabled', 'lightsailDistribution_isEnabled' - Indicates whether the distribution is enabled.
--
-- 'supportCode', 'lightsailDistribution_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail distribution. This code enables our
-- support team to look up your Lightsail information more easily.
--
-- 'defaultCacheBehavior', 'lightsailDistribution_defaultCacheBehavior' - An object that describes the default cache behavior of the distribution.
--
-- 'cacheBehaviors', 'lightsailDistribution_cacheBehaviors' - An array of objects that describe the per-path cache behavior of the
-- distribution.
--
-- 'tags', 'lightsailDistribution_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newLightsailDistribution ::
  LightsailDistribution
newLightsailDistribution =
  LightsailDistribution'
    { status = Prelude.Nothing,
      origin = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      cacheBehaviorSettings = Prelude.Nothing,
      alternativeDomainNames = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      ableToUpdateBundle = Prelude.Nothing,
      originPublicDNS = Prelude.Nothing,
      domainName = Prelude.Nothing,
      name = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      isEnabled = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing,
      cacheBehaviors = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the distribution.
lightsailDistribution_status :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_status = Lens.lens (\LightsailDistribution' {status} -> status) (\s@LightsailDistribution' {} a -> s {status = a} :: LightsailDistribution)

-- | An object that describes the origin resource of the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
lightsailDistribution_origin :: Lens.Lens' LightsailDistribution (Prelude.Maybe Origin)
lightsailDistribution_origin = Lens.lens (\LightsailDistribution' {origin} -> origin) (\s@LightsailDistribution' {} a -> s {origin = a} :: LightsailDistribution)

-- | The name of the SSL\/TLS certificate attached to the distribution, if
-- any.
lightsailDistribution_certificateName :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_certificateName = Lens.lens (\LightsailDistribution' {certificateName} -> certificateName) (\s@LightsailDistribution' {} a -> s {certificateName = a} :: LightsailDistribution)

-- | The Lightsail resource type (e.g., @Distribution@).
lightsailDistribution_resourceType :: Lens.Lens' LightsailDistribution (Prelude.Maybe ResourceType)
lightsailDistribution_resourceType = Lens.lens (\LightsailDistribution' {resourceType} -> resourceType) (\s@LightsailDistribution' {} a -> s {resourceType = a} :: LightsailDistribution)

-- | The Amazon Resource Name (ARN) of the distribution.
lightsailDistribution_arn :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_arn = Lens.lens (\LightsailDistribution' {arn} -> arn) (\s@LightsailDistribution' {} a -> s {arn = a} :: LightsailDistribution)

-- | The timestamp when the distribution was created.
lightsailDistribution_createdAt :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.UTCTime)
lightsailDistribution_createdAt = Lens.lens (\LightsailDistribution' {createdAt} -> createdAt) (\s@LightsailDistribution' {} a -> s {createdAt = a} :: LightsailDistribution) Prelude.. Lens.mapping Core._Time

-- | An object that describes the location of the distribution, such as the
-- AWS Region and Availability Zone.
--
-- Lightsail distributions are global resources that can reference an
-- origin in any AWS Region, and distribute its content globally. However,
-- all distributions are located in the @us-east-1@ Region.
lightsailDistribution_location :: Lens.Lens' LightsailDistribution (Prelude.Maybe ResourceLocation)
lightsailDistribution_location = Lens.lens (\LightsailDistribution' {location} -> location) (\s@LightsailDistribution' {} a -> s {location = a} :: LightsailDistribution)

-- | An object that describes the cache behavior settings of the
-- distribution.
lightsailDistribution_cacheBehaviorSettings :: Lens.Lens' LightsailDistribution (Prelude.Maybe CacheSettings)
lightsailDistribution_cacheBehaviorSettings = Lens.lens (\LightsailDistribution' {cacheBehaviorSettings} -> cacheBehaviorSettings) (\s@LightsailDistribution' {} a -> s {cacheBehaviorSettings = a} :: LightsailDistribution)

-- | The alternate domain names of the distribution.
lightsailDistribution_alternativeDomainNames :: Lens.Lens' LightsailDistribution (Prelude.Maybe [Prelude.Text])
lightsailDistribution_alternativeDomainNames = Lens.lens (\LightsailDistribution' {alternativeDomainNames} -> alternativeDomainNames) (\s@LightsailDistribution' {} a -> s {alternativeDomainNames = a} :: LightsailDistribution) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the bundle currently applied to the distribution.
lightsailDistribution_bundleId :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_bundleId = Lens.lens (\LightsailDistribution' {bundleId} -> bundleId) (\s@LightsailDistribution' {} a -> s {bundleId = a} :: LightsailDistribution)

-- | Indicates whether the bundle that is currently applied to your
-- distribution, specified using the @distributionName@ parameter, can be
-- changed to another bundle.
--
-- Use the @UpdateDistributionBundle@ action to change your distribution\'s
-- bundle.
lightsailDistribution_ableToUpdateBundle :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Bool)
lightsailDistribution_ableToUpdateBundle = Lens.lens (\LightsailDistribution' {ableToUpdateBundle} -> ableToUpdateBundle) (\s@LightsailDistribution' {} a -> s {ableToUpdateBundle = a} :: LightsailDistribution)

-- | The public DNS of the origin.
lightsailDistribution_originPublicDNS :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_originPublicDNS = Lens.lens (\LightsailDistribution' {originPublicDNS} -> originPublicDNS) (\s@LightsailDistribution' {} a -> s {originPublicDNS = a} :: LightsailDistribution)

-- | The domain name of the distribution.
lightsailDistribution_domainName :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_domainName = Lens.lens (\LightsailDistribution' {domainName} -> domainName) (\s@LightsailDistribution' {} a -> s {domainName = a} :: LightsailDistribution)

-- | The name of the distribution.
lightsailDistribution_name :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_name = Lens.lens (\LightsailDistribution' {name} -> name) (\s@LightsailDistribution' {} a -> s {name = a} :: LightsailDistribution)

-- | The IP address type of the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
lightsailDistribution_ipAddressType :: Lens.Lens' LightsailDistribution (Prelude.Maybe IpAddressType)
lightsailDistribution_ipAddressType = Lens.lens (\LightsailDistribution' {ipAddressType} -> ipAddressType) (\s@LightsailDistribution' {} a -> s {ipAddressType = a} :: LightsailDistribution)

-- | Indicates whether the distribution is enabled.
lightsailDistribution_isEnabled :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Bool)
lightsailDistribution_isEnabled = Lens.lens (\LightsailDistribution' {isEnabled} -> isEnabled) (\s@LightsailDistribution' {} a -> s {isEnabled = a} :: LightsailDistribution)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail distribution. This code enables our
-- support team to look up your Lightsail information more easily.
lightsailDistribution_supportCode :: Lens.Lens' LightsailDistribution (Prelude.Maybe Prelude.Text)
lightsailDistribution_supportCode = Lens.lens (\LightsailDistribution' {supportCode} -> supportCode) (\s@LightsailDistribution' {} a -> s {supportCode = a} :: LightsailDistribution)

-- | An object that describes the default cache behavior of the distribution.
lightsailDistribution_defaultCacheBehavior :: Lens.Lens' LightsailDistribution (Prelude.Maybe CacheBehavior)
lightsailDistribution_defaultCacheBehavior = Lens.lens (\LightsailDistribution' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@LightsailDistribution' {} a -> s {defaultCacheBehavior = a} :: LightsailDistribution)

-- | An array of objects that describe the per-path cache behavior of the
-- distribution.
lightsailDistribution_cacheBehaviors :: Lens.Lens' LightsailDistribution (Prelude.Maybe [CacheBehaviorPerPath])
lightsailDistribution_cacheBehaviors = Lens.lens (\LightsailDistribution' {cacheBehaviors} -> cacheBehaviors) (\s@LightsailDistribution' {} a -> s {cacheBehaviors = a} :: LightsailDistribution) Prelude.. Lens.mapping Lens.coerced

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
lightsailDistribution_tags :: Lens.Lens' LightsailDistribution (Prelude.Maybe [Tag])
lightsailDistribution_tags = Lens.lens (\LightsailDistribution' {tags} -> tags) (\s@LightsailDistribution' {} a -> s {tags = a} :: LightsailDistribution) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LightsailDistribution where
  parseJSON =
    Core.withObject
      "LightsailDistribution"
      ( \x ->
          LightsailDistribution'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "origin")
            Prelude.<*> (x Core..:? "certificateName")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "cacheBehaviorSettings")
            Prelude.<*> ( x Core..:? "alternativeDomainNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "ableToUpdateBundle")
            Prelude.<*> (x Core..:? "originPublicDNS")
            Prelude.<*> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "ipAddressType")
            Prelude.<*> (x Core..:? "isEnabled")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "defaultCacheBehavior")
            Prelude.<*> (x Core..:? "cacheBehaviors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable LightsailDistribution where
  hashWithSalt salt' LightsailDistribution' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cacheBehaviors
      `Prelude.hashWithSalt` defaultCacheBehavior
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` isEnabled
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` originPublicDNS
      `Prelude.hashWithSalt` ableToUpdateBundle
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` alternativeDomainNames
      `Prelude.hashWithSalt` cacheBehaviorSettings
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` status

instance Prelude.NFData LightsailDistribution where
  rnf LightsailDistribution' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cacheBehaviors
      `Prelude.seq` Prelude.rnf defaultCacheBehavior
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf isEnabled
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf originPublicDNS
      `Prelude.seq` Prelude.rnf ableToUpdateBundle
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf alternativeDomainNames
      `Prelude.seq` Prelude.rnf cacheBehaviorSettings
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf origin
