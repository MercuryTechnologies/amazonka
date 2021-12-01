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
-- Module      : Amazonka.Macie.Types.S3Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.S3Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the S3 resource. This data type is used as a
-- request parameter in the DisassociateS3Resources action and can be used
-- as a response parameter in the AssociateS3Resources and
-- UpdateS3Resources actions.
--
-- /See:/ 'newS3Resource' smart constructor.
data S3Resource = S3Resource'
  { -- | The prefix of the S3 bucket.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Resource_prefix' - The prefix of the S3 bucket.
--
-- 'bucketName', 's3Resource_bucketName' - The name of the S3 bucket.
newS3Resource ::
  -- | 'bucketName'
  Prelude.Text ->
  S3Resource
newS3Resource pBucketName_ =
  S3Resource'
    { prefix = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | The prefix of the S3 bucket.
s3Resource_prefix :: Lens.Lens' S3Resource (Prelude.Maybe Prelude.Text)
s3Resource_prefix = Lens.lens (\S3Resource' {prefix} -> prefix) (\s@S3Resource' {} a -> s {prefix = a} :: S3Resource)

-- | The name of the S3 bucket.
s3Resource_bucketName :: Lens.Lens' S3Resource Prelude.Text
s3Resource_bucketName = Lens.lens (\S3Resource' {bucketName} -> bucketName) (\s@S3Resource' {} a -> s {bucketName = a} :: S3Resource)

instance Core.FromJSON S3Resource where
  parseJSON =
    Core.withObject
      "S3Resource"
      ( \x ->
          S3Resource'
            Prelude.<$> (x Core..:? "prefix")
            Prelude.<*> (x Core..: "bucketName")
      )

instance Prelude.Hashable S3Resource where
  hashWithSalt salt' S3Resource' {..} =
    salt' `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Resource where
  rnf S3Resource' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName

instance Core.ToJSON S3Resource where
  toJSON S3Resource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefix" Core..=) Prelude.<$> prefix,
            Prelude.Just ("bucketName" Core..= bucketName)
          ]
      )
