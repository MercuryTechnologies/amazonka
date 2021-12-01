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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails

-- | A value to use for the filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
  { -- | A tag that is assigned to matching objects.
    tag :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails,
    -- | Prefix text for matching objects.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The type of filter value.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tag', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag' - A tag that is assigned to matching objects.
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix' - Prefix text for matching objects.
--
-- 'type'', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type' - The type of filter value.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
    { tag =
        Prelude.Nothing,
      prefix =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | A tag that is assigned to matching objects.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {tag} -> tag) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {tag = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

-- | Prefix text for matching objects.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

-- | The type of filter value.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {type'} -> type') (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {type' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
            Prelude.<$> (x Core..:? "Tag") Prelude.<*> (x Core..:? "Prefix")
              Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  hashWithSalt
    salt'
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      salt' `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` tag

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      Prelude.rnf tag `Prelude.seq` Prelude.rnf type'
        `Prelude.seq` Prelude.rnf prefix

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Tag" Core..=) Prelude.<$> tag,
              ("Prefix" Core..=) Prelude.<$> prefix,
              ("Type" Core..=) Prelude.<$> type'
            ]
        )
