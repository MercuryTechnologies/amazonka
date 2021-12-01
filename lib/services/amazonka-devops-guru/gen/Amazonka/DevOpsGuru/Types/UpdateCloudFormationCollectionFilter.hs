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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the names of AWS CloudFormation stacks used to update a
-- collection of stacks. You can specify up to 500 AWS CloudFormation
-- stacks.
--
-- /See:/ 'newUpdateCloudFormationCollectionFilter' smart constructor.
data UpdateCloudFormationCollectionFilter = UpdateCloudFormationCollectionFilter'
  { -- | An array of the names of the AWS CloudFormation stacks to update. You
    -- can specify up to 500 AWS CloudFormation stacks.
    stackNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCloudFormationCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackNames', 'updateCloudFormationCollectionFilter_stackNames' - An array of the names of the AWS CloudFormation stacks to update. You
-- can specify up to 500 AWS CloudFormation stacks.
newUpdateCloudFormationCollectionFilter ::
  UpdateCloudFormationCollectionFilter
newUpdateCloudFormationCollectionFilter =
  UpdateCloudFormationCollectionFilter'
    { stackNames =
        Prelude.Nothing
    }

-- | An array of the names of the AWS CloudFormation stacks to update. You
-- can specify up to 500 AWS CloudFormation stacks.
updateCloudFormationCollectionFilter_stackNames :: Lens.Lens' UpdateCloudFormationCollectionFilter (Prelude.Maybe [Prelude.Text])
updateCloudFormationCollectionFilter_stackNames = Lens.lens (\UpdateCloudFormationCollectionFilter' {stackNames} -> stackNames) (\s@UpdateCloudFormationCollectionFilter' {} a -> s {stackNames = a} :: UpdateCloudFormationCollectionFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    UpdateCloudFormationCollectionFilter
  where
  hashWithSalt
    salt'
    UpdateCloudFormationCollectionFilter' {..} =
      salt' `Prelude.hashWithSalt` stackNames

instance
  Prelude.NFData
    UpdateCloudFormationCollectionFilter
  where
  rnf UpdateCloudFormationCollectionFilter' {..} =
    Prelude.rnf stackNames

instance
  Core.ToJSON
    UpdateCloudFormationCollectionFilter
  where
  toJSON UpdateCloudFormationCollectionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [("StackNames" Core..=) Prelude.<$> stackNames]
      )
