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
-- Module      : Amazonka.Signer.Types.S3Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name and prefix of the S3 bucket where code signing saves your
-- signed objects.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | An Amazon S3 prefix that you can use to limit responses to those that
    -- begin with the specified prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Destination_prefix' - An Amazon S3 prefix that you can use to limit responses to those that
-- begin with the specified prefix.
--
-- 'bucketName', 's3Destination_bucketName' - Name of the S3 bucket.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { prefix = Prelude.Nothing,
      bucketName = Prelude.Nothing
    }

-- | An Amazon S3 prefix that you can use to limit responses to those that
-- begin with the specified prefix.
s3Destination_prefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_prefix = Lens.lens (\S3Destination' {prefix} -> prefix) (\s@S3Destination' {} a -> s {prefix = a} :: S3Destination)

-- | Name of the S3 bucket.
s3Destination_bucketName :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucketName = Lens.lens (\S3Destination' {bucketName} -> bucketName) (\s@S3Destination' {} a -> s {bucketName = a} :: S3Destination)

instance Prelude.Hashable S3Destination where
  hashWithSalt salt' S3Destination' {..} =
    salt' `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName

instance Core.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefix" Core..=) Prelude.<$> prefix,
            ("bucketName" Core..=) Prelude.<$> bucketName
          ]
      )
