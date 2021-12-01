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
-- Module      : Amazonka.AccessAnalyzer.Types.S3AccessPointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.S3AccessPointConfiguration where

import Amazonka.AccessAnalyzer.Types.NetworkOriginConfiguration
import Amazonka.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an Amazon S3 access point or multi-region access
-- point for the bucket. You can propose up to 10 access points or
-- multi-region access points per bucket. If the proposed Amazon S3 access
-- point configuration is for an existing bucket, the access preview uses
-- the proposed access point configuration in place of the existing access
-- points. To propose an access point without a policy, you can provide an
-- empty string as the access point policy. For more information, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonS3/latest/dev/creating-access-points.html Creating access points>.
-- For more information about access point policy limits, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points-restrictions-limitations.html Access points restrictions and limitations>.
--
-- /See:/ 'newS3AccessPointConfiguration' smart constructor.
data S3AccessPointConfiguration = S3AccessPointConfiguration'
  { -- | The proposed @S3PublicAccessBlock@ configuration to apply to this Amazon
    -- S3 access point or multi-region access point.
    publicAccessBlock :: Prelude.Maybe S3PublicAccessBlockConfiguration,
    -- | The access point or multi-region access point policy.
    accessPointPolicy :: Prelude.Maybe Prelude.Text,
    -- | The proposed @Internet@ and @VpcConfiguration@ to apply to this Amazon
    -- S3 access point. @VpcConfiguration@ does not apply to multi-region
    -- access points. If the access preview is for a new resource and neither
    -- is specified, the access preview uses @Internet@ for the network origin.
    -- If the access preview is for an existing resource and neither is
    -- specified, the access preview uses the exiting network origin.
    networkOrigin :: Prelude.Maybe NetworkOriginConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3AccessPointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicAccessBlock', 's3AccessPointConfiguration_publicAccessBlock' - The proposed @S3PublicAccessBlock@ configuration to apply to this Amazon
-- S3 access point or multi-region access point.
--
-- 'accessPointPolicy', 's3AccessPointConfiguration_accessPointPolicy' - The access point or multi-region access point policy.
--
-- 'networkOrigin', 's3AccessPointConfiguration_networkOrigin' - The proposed @Internet@ and @VpcConfiguration@ to apply to this Amazon
-- S3 access point. @VpcConfiguration@ does not apply to multi-region
-- access points. If the access preview is for a new resource and neither
-- is specified, the access preview uses @Internet@ for the network origin.
-- If the access preview is for an existing resource and neither is
-- specified, the access preview uses the exiting network origin.
newS3AccessPointConfiguration ::
  S3AccessPointConfiguration
newS3AccessPointConfiguration =
  S3AccessPointConfiguration'
    { publicAccessBlock =
        Prelude.Nothing,
      accessPointPolicy = Prelude.Nothing,
      networkOrigin = Prelude.Nothing
    }

-- | The proposed @S3PublicAccessBlock@ configuration to apply to this Amazon
-- S3 access point or multi-region access point.
s3AccessPointConfiguration_publicAccessBlock :: Lens.Lens' S3AccessPointConfiguration (Prelude.Maybe S3PublicAccessBlockConfiguration)
s3AccessPointConfiguration_publicAccessBlock = Lens.lens (\S3AccessPointConfiguration' {publicAccessBlock} -> publicAccessBlock) (\s@S3AccessPointConfiguration' {} a -> s {publicAccessBlock = a} :: S3AccessPointConfiguration)

-- | The access point or multi-region access point policy.
s3AccessPointConfiguration_accessPointPolicy :: Lens.Lens' S3AccessPointConfiguration (Prelude.Maybe Prelude.Text)
s3AccessPointConfiguration_accessPointPolicy = Lens.lens (\S3AccessPointConfiguration' {accessPointPolicy} -> accessPointPolicy) (\s@S3AccessPointConfiguration' {} a -> s {accessPointPolicy = a} :: S3AccessPointConfiguration)

-- | The proposed @Internet@ and @VpcConfiguration@ to apply to this Amazon
-- S3 access point. @VpcConfiguration@ does not apply to multi-region
-- access points. If the access preview is for a new resource and neither
-- is specified, the access preview uses @Internet@ for the network origin.
-- If the access preview is for an existing resource and neither is
-- specified, the access preview uses the exiting network origin.
s3AccessPointConfiguration_networkOrigin :: Lens.Lens' S3AccessPointConfiguration (Prelude.Maybe NetworkOriginConfiguration)
s3AccessPointConfiguration_networkOrigin = Lens.lens (\S3AccessPointConfiguration' {networkOrigin} -> networkOrigin) (\s@S3AccessPointConfiguration' {} a -> s {networkOrigin = a} :: S3AccessPointConfiguration)

instance Core.FromJSON S3AccessPointConfiguration where
  parseJSON =
    Core.withObject
      "S3AccessPointConfiguration"
      ( \x ->
          S3AccessPointConfiguration'
            Prelude.<$> (x Core..:? "publicAccessBlock")
            Prelude.<*> (x Core..:? "accessPointPolicy")
            Prelude.<*> (x Core..:? "networkOrigin")
      )

instance Prelude.Hashable S3AccessPointConfiguration where
  hashWithSalt salt' S3AccessPointConfiguration' {..} =
    salt' `Prelude.hashWithSalt` networkOrigin
      `Prelude.hashWithSalt` accessPointPolicy
      `Prelude.hashWithSalt` publicAccessBlock

instance Prelude.NFData S3AccessPointConfiguration where
  rnf S3AccessPointConfiguration' {..} =
    Prelude.rnf publicAccessBlock
      `Prelude.seq` Prelude.rnf networkOrigin
      `Prelude.seq` Prelude.rnf accessPointPolicy

instance Core.ToJSON S3AccessPointConfiguration where
  toJSON S3AccessPointConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("publicAccessBlock" Core..=)
              Prelude.<$> publicAccessBlock,
            ("accessPointPolicy" Core..=)
              Prelude.<$> accessPointPolicy,
            ("networkOrigin" Core..=) Prelude.<$> networkOrigin
          ]
      )
