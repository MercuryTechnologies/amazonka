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
-- Module      : Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an AWS CloudFormation stack used to create a monthly
-- cost estimate for DevOps Guru to analyze AWS resources. The maximum
-- number of stacks you can specify for a cost estimate is one. The
-- estimate created is for the cost to analyze the AWS resources defined by
-- the stack. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /AWS CloudFormation User Guide/.
--
-- /See:/ 'newCloudFormationCostEstimationResourceCollectionFilter' smart constructor.
data CloudFormationCostEstimationResourceCollectionFilter = CloudFormationCostEstimationResourceCollectionFilter'
  { -- | An array of CloudFormation stack names. Its size is fixed at 1 item.
    stackNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationCostEstimationResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackNames', 'cloudFormationCostEstimationResourceCollectionFilter_stackNames' - An array of CloudFormation stack names. Its size is fixed at 1 item.
newCloudFormationCostEstimationResourceCollectionFilter ::
  CloudFormationCostEstimationResourceCollectionFilter
newCloudFormationCostEstimationResourceCollectionFilter =
  CloudFormationCostEstimationResourceCollectionFilter'
    { stackNames =
        Prelude.Nothing
    }

-- | An array of CloudFormation stack names. Its size is fixed at 1 item.
cloudFormationCostEstimationResourceCollectionFilter_stackNames :: Lens.Lens' CloudFormationCostEstimationResourceCollectionFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
cloudFormationCostEstimationResourceCollectionFilter_stackNames = Lens.lens (\CloudFormationCostEstimationResourceCollectionFilter' {stackNames} -> stackNames) (\s@CloudFormationCostEstimationResourceCollectionFilter' {} a -> s {stackNames = a} :: CloudFormationCostEstimationResourceCollectionFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    CloudFormationCostEstimationResourceCollectionFilter
  where
  parseJSON =
    Core.withObject
      "CloudFormationCostEstimationResourceCollectionFilter"
      ( \x ->
          CloudFormationCostEstimationResourceCollectionFilter'
            Prelude.<$> (x Core..:? "StackNames")
      )

instance
  Prelude.Hashable
    CloudFormationCostEstimationResourceCollectionFilter
  where
  hashWithSalt
    salt'
    CloudFormationCostEstimationResourceCollectionFilter' {..} =
      salt' `Prelude.hashWithSalt` stackNames

instance
  Prelude.NFData
    CloudFormationCostEstimationResourceCollectionFilter
  where
  rnf
    CloudFormationCostEstimationResourceCollectionFilter' {..} =
      Prelude.rnf stackNames

instance
  Core.ToJSON
    CloudFormationCostEstimationResourceCollectionFilter
  where
  toJSON
    CloudFormationCostEstimationResourceCollectionFilter' {..} =
      Core.object
        ( Prelude.catMaybes
            [("StackNames" Core..=) Prelude.<$> stackNames]
        )
