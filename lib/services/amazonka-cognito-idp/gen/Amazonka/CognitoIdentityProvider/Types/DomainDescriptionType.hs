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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DomainDescriptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DomainDescriptionType where

import Amazonka.CognitoIdentityProvider.Types.CustomDomainConfigType
import Amazonka.CognitoIdentityProvider.Types.DomainStatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A container for information about a domain.
--
-- /See:/ 'newDomainDescriptionType' smart constructor.
data DomainDescriptionType = DomainDescriptionType'
  { -- | The domain status.
    status :: Prelude.Maybe DomainStatusType,
    -- | The ARN of the CloudFront distribution.
    cloudFrontDistribution :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The domain string.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The account ID for the user pool owner.
    aWSAccountId :: Prelude.Maybe Prelude.Text,
    -- | The configuration for a custom domain that hosts the sign-up and sign-in
    -- webpages for your application.
    customDomainConfig :: Prelude.Maybe CustomDomainConfigType,
    -- | The app version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where the static files for this domain are stored.
    s3Bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDescriptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'domainDescriptionType_status' - The domain status.
--
-- 'cloudFrontDistribution', 'domainDescriptionType_cloudFrontDistribution' - The ARN of the CloudFront distribution.
--
-- 'userPoolId', 'domainDescriptionType_userPoolId' - The user pool ID.
--
-- 'domain', 'domainDescriptionType_domain' - The domain string.
--
-- 'aWSAccountId', 'domainDescriptionType_aWSAccountId' - The account ID for the user pool owner.
--
-- 'customDomainConfig', 'domainDescriptionType_customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
--
-- 'version', 'domainDescriptionType_version' - The app version.
--
-- 's3Bucket', 'domainDescriptionType_s3Bucket' - The S3 bucket where the static files for this domain are stored.
newDomainDescriptionType ::
  DomainDescriptionType
newDomainDescriptionType =
  DomainDescriptionType'
    { status = Prelude.Nothing,
      cloudFrontDistribution = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      domain = Prelude.Nothing,
      aWSAccountId = Prelude.Nothing,
      customDomainConfig = Prelude.Nothing,
      version = Prelude.Nothing,
      s3Bucket = Prelude.Nothing
    }

-- | The domain status.
domainDescriptionType_status :: Lens.Lens' DomainDescriptionType (Prelude.Maybe DomainStatusType)
domainDescriptionType_status = Lens.lens (\DomainDescriptionType' {status} -> status) (\s@DomainDescriptionType' {} a -> s {status = a} :: DomainDescriptionType)

-- | The ARN of the CloudFront distribution.
domainDescriptionType_cloudFrontDistribution :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_cloudFrontDistribution = Lens.lens (\DomainDescriptionType' {cloudFrontDistribution} -> cloudFrontDistribution) (\s@DomainDescriptionType' {} a -> s {cloudFrontDistribution = a} :: DomainDescriptionType)

-- | The user pool ID.
domainDescriptionType_userPoolId :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_userPoolId = Lens.lens (\DomainDescriptionType' {userPoolId} -> userPoolId) (\s@DomainDescriptionType' {} a -> s {userPoolId = a} :: DomainDescriptionType)

-- | The domain string.
domainDescriptionType_domain :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_domain = Lens.lens (\DomainDescriptionType' {domain} -> domain) (\s@DomainDescriptionType' {} a -> s {domain = a} :: DomainDescriptionType)

-- | The account ID for the user pool owner.
domainDescriptionType_aWSAccountId :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_aWSAccountId = Lens.lens (\DomainDescriptionType' {aWSAccountId} -> aWSAccountId) (\s@DomainDescriptionType' {} a -> s {aWSAccountId = a} :: DomainDescriptionType)

-- | The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
domainDescriptionType_customDomainConfig :: Lens.Lens' DomainDescriptionType (Prelude.Maybe CustomDomainConfigType)
domainDescriptionType_customDomainConfig = Lens.lens (\DomainDescriptionType' {customDomainConfig} -> customDomainConfig) (\s@DomainDescriptionType' {} a -> s {customDomainConfig = a} :: DomainDescriptionType)

-- | The app version.
domainDescriptionType_version :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_version = Lens.lens (\DomainDescriptionType' {version} -> version) (\s@DomainDescriptionType' {} a -> s {version = a} :: DomainDescriptionType)

-- | The S3 bucket where the static files for this domain are stored.
domainDescriptionType_s3Bucket :: Lens.Lens' DomainDescriptionType (Prelude.Maybe Prelude.Text)
domainDescriptionType_s3Bucket = Lens.lens (\DomainDescriptionType' {s3Bucket} -> s3Bucket) (\s@DomainDescriptionType' {} a -> s {s3Bucket = a} :: DomainDescriptionType)

instance Core.FromJSON DomainDescriptionType where
  parseJSON =
    Core.withObject
      "DomainDescriptionType"
      ( \x ->
          DomainDescriptionType'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CloudFrontDistribution")
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "AWSAccountId")
            Prelude.<*> (x Core..:? "CustomDomainConfig")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "S3Bucket")
      )

instance Prelude.Hashable DomainDescriptionType where
  hashWithSalt salt' DomainDescriptionType' {..} =
    salt' `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` customDomainConfig
      `Prelude.hashWithSalt` aWSAccountId
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` cloudFrontDistribution
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainDescriptionType where
  rnf DomainDescriptionType' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf customDomainConfig
      `Prelude.seq` Prelude.rnf aWSAccountId
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf cloudFrontDistribution
