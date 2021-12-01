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
-- Module      : Amazonka.MacieV2.Types.BucketCountBySharedAccessType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketCountBySharedAccessType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the number of S3 buckets that are or aren\'t
-- shared with other Amazon Web Services accounts.
--
-- /See:/ 'newBucketCountBySharedAccessType' smart constructor.
data BucketCountBySharedAccessType = BucketCountBySharedAccessType'
  { -- | The total number of buckets that aren\'t shared with other Amazon Web
    -- Services accounts.
    notShared :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that are shared with an Amazon Web Services
    -- account that\'s part of the same Amazon Macie organization.
    internal :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that are shared with an Amazon Web Services
    -- account that isn\'t part of the same Amazon Macie organization.
    external :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
    -- shared access settings for. Macie can\'t determine whether these buckets
    -- are shared with other Amazon Web Services accounts.
    unknown :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketCountBySharedAccessType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notShared', 'bucketCountBySharedAccessType_notShared' - The total number of buckets that aren\'t shared with other Amazon Web
-- Services accounts.
--
-- 'internal', 'bucketCountBySharedAccessType_internal' - The total number of buckets that are shared with an Amazon Web Services
-- account that\'s part of the same Amazon Macie organization.
--
-- 'external', 'bucketCountBySharedAccessType_external' - The total number of buckets that are shared with an Amazon Web Services
-- account that isn\'t part of the same Amazon Macie organization.
--
-- 'unknown', 'bucketCountBySharedAccessType_unknown' - The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- shared access settings for. Macie can\'t determine whether these buckets
-- are shared with other Amazon Web Services accounts.
newBucketCountBySharedAccessType ::
  BucketCountBySharedAccessType
newBucketCountBySharedAccessType =
  BucketCountBySharedAccessType'
    { notShared =
        Prelude.Nothing,
      internal = Prelude.Nothing,
      external = Prelude.Nothing,
      unknown = Prelude.Nothing
    }

-- | The total number of buckets that aren\'t shared with other Amazon Web
-- Services accounts.
bucketCountBySharedAccessType_notShared :: Lens.Lens' BucketCountBySharedAccessType (Prelude.Maybe Prelude.Integer)
bucketCountBySharedAccessType_notShared = Lens.lens (\BucketCountBySharedAccessType' {notShared} -> notShared) (\s@BucketCountBySharedAccessType' {} a -> s {notShared = a} :: BucketCountBySharedAccessType)

-- | The total number of buckets that are shared with an Amazon Web Services
-- account that\'s part of the same Amazon Macie organization.
bucketCountBySharedAccessType_internal :: Lens.Lens' BucketCountBySharedAccessType (Prelude.Maybe Prelude.Integer)
bucketCountBySharedAccessType_internal = Lens.lens (\BucketCountBySharedAccessType' {internal} -> internal) (\s@BucketCountBySharedAccessType' {} a -> s {internal = a} :: BucketCountBySharedAccessType)

-- | The total number of buckets that are shared with an Amazon Web Services
-- account that isn\'t part of the same Amazon Macie organization.
bucketCountBySharedAccessType_external :: Lens.Lens' BucketCountBySharedAccessType (Prelude.Maybe Prelude.Integer)
bucketCountBySharedAccessType_external = Lens.lens (\BucketCountBySharedAccessType' {external} -> external) (\s@BucketCountBySharedAccessType' {} a -> s {external = a} :: BucketCountBySharedAccessType)

-- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- shared access settings for. Macie can\'t determine whether these buckets
-- are shared with other Amazon Web Services accounts.
bucketCountBySharedAccessType_unknown :: Lens.Lens' BucketCountBySharedAccessType (Prelude.Maybe Prelude.Integer)
bucketCountBySharedAccessType_unknown = Lens.lens (\BucketCountBySharedAccessType' {unknown} -> unknown) (\s@BucketCountBySharedAccessType' {} a -> s {unknown = a} :: BucketCountBySharedAccessType)

instance Core.FromJSON BucketCountBySharedAccessType where
  parseJSON =
    Core.withObject
      "BucketCountBySharedAccessType"
      ( \x ->
          BucketCountBySharedAccessType'
            Prelude.<$> (x Core..:? "notShared")
            Prelude.<*> (x Core..:? "internal")
            Prelude.<*> (x Core..:? "external")
            Prelude.<*> (x Core..:? "unknown")
      )

instance
  Prelude.Hashable
    BucketCountBySharedAccessType
  where
  hashWithSalt salt' BucketCountBySharedAccessType' {..} =
    salt' `Prelude.hashWithSalt` unknown
      `Prelude.hashWithSalt` external
      `Prelude.hashWithSalt` internal
      `Prelude.hashWithSalt` notShared

instance Prelude.NFData BucketCountBySharedAccessType where
  rnf BucketCountBySharedAccessType' {..} =
    Prelude.rnf notShared
      `Prelude.seq` Prelude.rnf unknown
      `Prelude.seq` Prelude.rnf external
      `Prelude.seq` Prelude.rnf internal
