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
-- Module      : Amazonka.AppFlow.Types.S3DestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3DestinationProperties where

import Amazonka.AppFlow.Types.S3OutputFormatConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon S3 is used as a destination.
--
-- /See:/ 'newS3DestinationProperties' smart constructor.
data S3DestinationProperties = S3DestinationProperties'
  { -- | The object key for the destination bucket in which Amazon AppFlow places
    -- the files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    s3OutputFormatConfig :: Prelude.Maybe S3OutputFormatConfig,
    -- | The Amazon S3 bucket name in which Amazon AppFlow places the transferred
    -- data.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketPrefix', 's3DestinationProperties_bucketPrefix' - The object key for the destination bucket in which Amazon AppFlow places
-- the files.
--
-- 's3OutputFormatConfig', 's3DestinationProperties_s3OutputFormatConfig' - Undocumented member.
--
-- 'bucketName', 's3DestinationProperties_bucketName' - The Amazon S3 bucket name in which Amazon AppFlow places the transferred
-- data.
newS3DestinationProperties ::
  -- | 'bucketName'
  Prelude.Text ->
  S3DestinationProperties
newS3DestinationProperties pBucketName_ =
  S3DestinationProperties'
    { bucketPrefix =
        Prelude.Nothing,
      s3OutputFormatConfig = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | The object key for the destination bucket in which Amazon AppFlow places
-- the files.
s3DestinationProperties_bucketPrefix :: Lens.Lens' S3DestinationProperties (Prelude.Maybe Prelude.Text)
s3DestinationProperties_bucketPrefix = Lens.lens (\S3DestinationProperties' {bucketPrefix} -> bucketPrefix) (\s@S3DestinationProperties' {} a -> s {bucketPrefix = a} :: S3DestinationProperties)

-- | Undocumented member.
s3DestinationProperties_s3OutputFormatConfig :: Lens.Lens' S3DestinationProperties (Prelude.Maybe S3OutputFormatConfig)
s3DestinationProperties_s3OutputFormatConfig = Lens.lens (\S3DestinationProperties' {s3OutputFormatConfig} -> s3OutputFormatConfig) (\s@S3DestinationProperties' {} a -> s {s3OutputFormatConfig = a} :: S3DestinationProperties)

-- | The Amazon S3 bucket name in which Amazon AppFlow places the transferred
-- data.
s3DestinationProperties_bucketName :: Lens.Lens' S3DestinationProperties Prelude.Text
s3DestinationProperties_bucketName = Lens.lens (\S3DestinationProperties' {bucketName} -> bucketName) (\s@S3DestinationProperties' {} a -> s {bucketName = a} :: S3DestinationProperties)

instance Core.FromJSON S3DestinationProperties where
  parseJSON =
    Core.withObject
      "S3DestinationProperties"
      ( \x ->
          S3DestinationProperties'
            Prelude.<$> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..:? "s3OutputFormatConfig")
            Prelude.<*> (x Core..: "bucketName")
      )

instance Prelude.Hashable S3DestinationProperties where
  hashWithSalt salt' S3DestinationProperties' {..} =
    salt' `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` s3OutputFormatConfig
      `Prelude.hashWithSalt` bucketPrefix

instance Prelude.NFData S3DestinationProperties where
  rnf S3DestinationProperties' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf s3OutputFormatConfig

instance Core.ToJSON S3DestinationProperties where
  toJSON S3DestinationProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Core..=) Prelude.<$> bucketPrefix,
            ("s3OutputFormatConfig" Core..=)
              Prelude.<$> s3OutputFormatConfig,
            Prelude.Just ("bucketName" Core..= bucketName)
          ]
      )
