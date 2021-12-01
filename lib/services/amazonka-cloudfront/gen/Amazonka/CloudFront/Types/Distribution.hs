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
-- Module      : Amazonka.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Distribution where

import Amazonka.CloudFront.Types.ActiveTrustedKeyGroups
import Amazonka.CloudFront.Types.ActiveTrustedSigners
import Amazonka.CloudFront.Types.AliasICPRecordal
import Amazonka.CloudFront.Types.DistributionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A distribution tells CloudFront where you want content to be delivered
-- from, and the details about how to track and manage content delivery.
--
-- /See:/ 'newDistribution' smart constructor.
data Distribution = Distribution'
  { -- | CloudFront automatically adds this field to the response if you’ve
    -- configured a cache behavior in this distribution to serve private
    -- content using key groups. This field contains a list of key groups and
    -- the public keys in each key group that CloudFront can use to verify the
    -- signatures of signed URLs or signed cookies.
    activeTrustedKeyGroups :: Prelude.Maybe ActiveTrustedKeyGroups,
    -- | Amazon Web Services services in China customers must file for an
    -- Internet Content Provider (ICP) recordal if they want to serve content
    -- publicly on an alternate domain name, also known as a CNAME, that
    -- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
    -- status for CNAMEs associated with distributions.
    --
    -- For more information about ICP recordals, see
    -- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
    -- in /Getting Started with Amazon Web Services services in China/.
    aliasICPRecordals :: Prelude.Maybe [AliasICPRecordal],
    -- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
    --
    -- CloudFront automatically adds this field to the response if you’ve
    -- configured a cache behavior in this distribution to serve private
    -- content using trusted signers. This field contains a list of account IDs
    -- and the active CloudFront key pairs in each account that CloudFront can
    -- use to verify the signatures of signed URLs or signed cookies.
    activeTrustedSigners :: Prelude.Maybe ActiveTrustedSigners,
    -- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
    id :: Prelude.Text,
    -- | The ARN (Amazon Resource Name) for the distribution. For example:
    -- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
    -- @123456789012@ is your account ID.
    arn :: Prelude.Text,
    -- | This response element indicates the current status of the distribution.
    -- When the status is @Deployed@, the distribution\'s information is fully
    -- propagated to all CloudFront edge locations.
    status :: Prelude.Text,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Core.ISO8601,
    -- | The number of invalidation batches currently in progress.
    inProgressInvalidationBatches :: Prelude.Int,
    -- | The domain name corresponding to the distribution, for example,
    -- @d111111abcdef8.cloudfront.net@.
    domainName :: Prelude.Text,
    -- | The current configuration information for the distribution. Send a @GET@
    -- request to the @\/CloudFront API version\/distribution ID\/config@
    -- resource.
    distributionConfig :: DistributionConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Distribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeTrustedKeyGroups', 'distribution_activeTrustedKeyGroups' - CloudFront automatically adds this field to the response if you’ve
-- configured a cache behavior in this distribution to serve private
-- content using key groups. This field contains a list of key groups and
-- the public keys in each key group that CloudFront can use to verify the
-- signatures of signed URLs or signed cookies.
--
-- 'aliasICPRecordals', 'distribution_aliasICPRecordals' - Amazon Web Services services in China customers must file for an
-- Internet Content Provider (ICP) recordal if they want to serve content
-- publicly on an alternate domain name, also known as a CNAME, that
-- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
-- status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with Amazon Web Services services in China/.
--
-- 'activeTrustedSigners', 'distribution_activeTrustedSigners' - We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- CloudFront automatically adds this field to the response if you’ve
-- configured a cache behavior in this distribution to serve private
-- content using trusted signers. This field contains a list of account IDs
-- and the active CloudFront key pairs in each account that CloudFront can
-- use to verify the signatures of signed URLs or signed cookies.
--
-- 'id', 'distribution_id' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
--
-- 'arn', 'distribution_arn' - The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your account ID.
--
-- 'status', 'distribution_status' - This response element indicates the current status of the distribution.
-- When the status is @Deployed@, the distribution\'s information is fully
-- propagated to all CloudFront edge locations.
--
-- 'lastModifiedTime', 'distribution_lastModifiedTime' - The date and time the distribution was last modified.
--
-- 'inProgressInvalidationBatches', 'distribution_inProgressInvalidationBatches' - The number of invalidation batches currently in progress.
--
-- 'domainName', 'distribution_domainName' - The domain name corresponding to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
--
-- 'distributionConfig', 'distribution_distributionConfig' - The current configuration information for the distribution. Send a @GET@
-- request to the @\/CloudFront API version\/distribution ID\/config@
-- resource.
newDistribution ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'inProgressInvalidationBatches'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'distributionConfig'
  DistributionConfig ->
  Distribution
newDistribution
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pInProgressInvalidationBatches_
  pDomainName_
  pDistributionConfig_ =
    Distribution'
      { activeTrustedKeyGroups =
          Prelude.Nothing,
        aliasICPRecordals = Prelude.Nothing,
        activeTrustedSigners = Prelude.Nothing,
        id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        inProgressInvalidationBatches =
          pInProgressInvalidationBatches_,
        domainName = pDomainName_,
        distributionConfig = pDistributionConfig_
      }

-- | CloudFront automatically adds this field to the response if you’ve
-- configured a cache behavior in this distribution to serve private
-- content using key groups. This field contains a list of key groups and
-- the public keys in each key group that CloudFront can use to verify the
-- signatures of signed URLs or signed cookies.
distribution_activeTrustedKeyGroups :: Lens.Lens' Distribution (Prelude.Maybe ActiveTrustedKeyGroups)
distribution_activeTrustedKeyGroups = Lens.lens (\Distribution' {activeTrustedKeyGroups} -> activeTrustedKeyGroups) (\s@Distribution' {} a -> s {activeTrustedKeyGroups = a} :: Distribution)

-- | Amazon Web Services services in China customers must file for an
-- Internet Content Provider (ICP) recordal if they want to serve content
-- publicly on an alternate domain name, also known as a CNAME, that
-- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
-- status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with Amazon Web Services services in China/.
distribution_aliasICPRecordals :: Lens.Lens' Distribution (Prelude.Maybe [AliasICPRecordal])
distribution_aliasICPRecordals = Lens.lens (\Distribution' {aliasICPRecordals} -> aliasICPRecordals) (\s@Distribution' {} a -> s {aliasICPRecordals = a} :: Distribution) Prelude.. Lens.mapping Lens.coerced

-- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- CloudFront automatically adds this field to the response if you’ve
-- configured a cache behavior in this distribution to serve private
-- content using trusted signers. This field contains a list of account IDs
-- and the active CloudFront key pairs in each account that CloudFront can
-- use to verify the signatures of signed URLs or signed cookies.
distribution_activeTrustedSigners :: Lens.Lens' Distribution (Prelude.Maybe ActiveTrustedSigners)
distribution_activeTrustedSigners = Lens.lens (\Distribution' {activeTrustedSigners} -> activeTrustedSigners) (\s@Distribution' {} a -> s {activeTrustedSigners = a} :: Distribution)

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
distribution_id :: Lens.Lens' Distribution Prelude.Text
distribution_id = Lens.lens (\Distribution' {id} -> id) (\s@Distribution' {} a -> s {id = a} :: Distribution)

-- | The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your account ID.
distribution_arn :: Lens.Lens' Distribution Prelude.Text
distribution_arn = Lens.lens (\Distribution' {arn} -> arn) (\s@Distribution' {} a -> s {arn = a} :: Distribution)

-- | This response element indicates the current status of the distribution.
-- When the status is @Deployed@, the distribution\'s information is fully
-- propagated to all CloudFront edge locations.
distribution_status :: Lens.Lens' Distribution Prelude.Text
distribution_status = Lens.lens (\Distribution' {status} -> status) (\s@Distribution' {} a -> s {status = a} :: Distribution)

-- | The date and time the distribution was last modified.
distribution_lastModifiedTime :: Lens.Lens' Distribution Prelude.UTCTime
distribution_lastModifiedTime = Lens.lens (\Distribution' {lastModifiedTime} -> lastModifiedTime) (\s@Distribution' {} a -> s {lastModifiedTime = a} :: Distribution) Prelude.. Core._Time

-- | The number of invalidation batches currently in progress.
distribution_inProgressInvalidationBatches :: Lens.Lens' Distribution Prelude.Int
distribution_inProgressInvalidationBatches = Lens.lens (\Distribution' {inProgressInvalidationBatches} -> inProgressInvalidationBatches) (\s@Distribution' {} a -> s {inProgressInvalidationBatches = a} :: Distribution)

-- | The domain name corresponding to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
distribution_domainName :: Lens.Lens' Distribution Prelude.Text
distribution_domainName = Lens.lens (\Distribution' {domainName} -> domainName) (\s@Distribution' {} a -> s {domainName = a} :: Distribution)

-- | The current configuration information for the distribution. Send a @GET@
-- request to the @\/CloudFront API version\/distribution ID\/config@
-- resource.
distribution_distributionConfig :: Lens.Lens' Distribution DistributionConfig
distribution_distributionConfig = Lens.lens (\Distribution' {distributionConfig} -> distributionConfig) (\s@Distribution' {} a -> s {distributionConfig = a} :: Distribution)

instance Core.FromXML Distribution where
  parseXML x =
    Distribution'
      Prelude.<$> (x Core..@? "ActiveTrustedKeyGroups")
      Prelude.<*> ( x Core..@? "AliasICPRecordals"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AliasICPRecordal")
                  )
      Prelude.<*> (x Core..@? "ActiveTrustedSigners")
      Prelude.<*> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "ARN")
      Prelude.<*> (x Core..@ "Status")
      Prelude.<*> (x Core..@ "LastModifiedTime")
      Prelude.<*> (x Core..@ "InProgressInvalidationBatches")
      Prelude.<*> (x Core..@ "DomainName")
      Prelude.<*> (x Core..@ "DistributionConfig")

instance Prelude.Hashable Distribution where
  hashWithSalt salt' Distribution' {..} =
    salt' `Prelude.hashWithSalt` distributionConfig
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` inProgressInvalidationBatches
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` activeTrustedSigners
      `Prelude.hashWithSalt` aliasICPRecordals
      `Prelude.hashWithSalt` activeTrustedKeyGroups

instance Prelude.NFData Distribution where
  rnf Distribution' {..} =
    Prelude.rnf activeTrustedKeyGroups
      `Prelude.seq` Prelude.rnf distributionConfig
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf inProgressInvalidationBatches
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf activeTrustedSigners
      `Prelude.seq` Prelude.rnf aliasICPRecordals
