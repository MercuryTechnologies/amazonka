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
-- Module      : Amazonka.DataSync.Types.S3Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.S3Config where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that is used to access an Amazon S3 bucket.
--
-- For detailed information about using such a role, see Creating a
-- Location for Amazon S3 in the /DataSync User Guide/.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The Amazon S3 bucket to access. This bucket is used as a parameter in
    -- the
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationS3.html CreateLocationS3>
    -- operation.
    bucketAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketAccessRoleArn', 's3Config_bucketAccessRoleArn' - The Amazon S3 bucket to access. This bucket is used as a parameter in
-- the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationS3.html CreateLocationS3>
-- operation.
newS3Config ::
  -- | 'bucketAccessRoleArn'
  Prelude.Text ->
  S3Config
newS3Config pBucketAccessRoleArn_ =
  S3Config'
    { bucketAccessRoleArn =
        pBucketAccessRoleArn_
    }

-- | The Amazon S3 bucket to access. This bucket is used as a parameter in
-- the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateLocationS3.html CreateLocationS3>
-- operation.
s3Config_bucketAccessRoleArn :: Lens.Lens' S3Config Prelude.Text
s3Config_bucketAccessRoleArn = Lens.lens (\S3Config' {bucketAccessRoleArn} -> bucketAccessRoleArn) (\s@S3Config' {} a -> s {bucketAccessRoleArn = a} :: S3Config)

instance Core.FromJSON S3Config where
  parseJSON =
    Core.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Core..: "BucketAccessRoleArn")
      )

instance Prelude.Hashable S3Config where
  hashWithSalt salt' S3Config' {..} =
    salt' `Prelude.hashWithSalt` bucketAccessRoleArn

instance Prelude.NFData S3Config where
  rnf S3Config' {..} = Prelude.rnf bucketAccessRoleArn

instance Core.ToJSON S3Config where
  toJSON S3Config' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BucketAccessRoleArn" Core..= bucketAccessRoleArn)
          ]
      )
