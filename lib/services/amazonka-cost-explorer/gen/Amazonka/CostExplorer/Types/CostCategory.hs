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
-- Module      : Amazonka.CostExplorer.Types.CostCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategory where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.CostCategoryProcessingStatus
import Amazonka.CostExplorer.Types.CostCategoryRule
import Amazonka.CostExplorer.Types.CostCategoryRuleVersion
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRule
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structure of Cost Categories. This includes detailed metadata and
-- the set of rules for the @CostCategory@ object.
--
-- /See:/ 'newCostCategory' smart constructor.
data CostCategory = CostCategory'
  { -- | The list of processing statuses for Cost Management products for a
    -- specific cost category.
    processingStatus :: Prelude.Maybe [CostCategoryProcessingStatus],
    -- | The effective end data of your Cost Category.
    effectiveEnd :: Prelude.Maybe Prelude.Text,
    -- | The split charge rules that are used to allocate your charges between
    -- your Cost Category values.
    splitChargeRules :: Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRule),
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for your Cost Category.
    costCategoryArn :: Prelude.Text,
    -- | The effective state data of your Cost Category.
    effectiveStart :: Prelude.Text,
    name :: Prelude.Text,
    ruleVersion :: CostCategoryRuleVersion,
    -- | The rules are processed in order. If there are multiple rules that match
    -- the line item, then the first rule to match is used to determine that
    -- Cost Category value.
    rules :: Prelude.NonEmpty CostCategoryRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingStatus', 'costCategory_processingStatus' - The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- 'effectiveEnd', 'costCategory_effectiveEnd' - The effective end data of your Cost Category.
--
-- 'splitChargeRules', 'costCategory_splitChargeRules' - The split charge rules that are used to allocate your charges between
-- your Cost Category values.
--
-- 'defaultValue', 'costCategory_defaultValue' - Undocumented member.
--
-- 'costCategoryArn', 'costCategory_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'effectiveStart', 'costCategory_effectiveStart' - The effective state data of your Cost Category.
--
-- 'name', 'costCategory_name' - Undocumented member.
--
-- 'ruleVersion', 'costCategory_ruleVersion' - Undocumented member.
--
-- 'rules', 'costCategory_rules' - The rules are processed in order. If there are multiple rules that match
-- the line item, then the first rule to match is used to determine that
-- Cost Category value.
newCostCategory ::
  -- | 'costCategoryArn'
  Prelude.Text ->
  -- | 'effectiveStart'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Prelude.NonEmpty CostCategoryRule ->
  CostCategory
newCostCategory
  pCostCategoryArn_
  pEffectiveStart_
  pName_
  pRuleVersion_
  pRules_ =
    CostCategory'
      { processingStatus = Prelude.Nothing,
        effectiveEnd = Prelude.Nothing,
        splitChargeRules = Prelude.Nothing,
        defaultValue = Prelude.Nothing,
        costCategoryArn = pCostCategoryArn_,
        effectiveStart = pEffectiveStart_,
        name = pName_,
        ruleVersion = pRuleVersion_,
        rules = Lens.coerced Lens.# pRules_
      }

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
costCategory_processingStatus :: Lens.Lens' CostCategory (Prelude.Maybe [CostCategoryProcessingStatus])
costCategory_processingStatus = Lens.lens (\CostCategory' {processingStatus} -> processingStatus) (\s@CostCategory' {} a -> s {processingStatus = a} :: CostCategory) Prelude.. Lens.mapping Lens.coerced

-- | The effective end data of your Cost Category.
costCategory_effectiveEnd :: Lens.Lens' CostCategory (Prelude.Maybe Prelude.Text)
costCategory_effectiveEnd = Lens.lens (\CostCategory' {effectiveEnd} -> effectiveEnd) (\s@CostCategory' {} a -> s {effectiveEnd = a} :: CostCategory)

-- | The split charge rules that are used to allocate your charges between
-- your Cost Category values.
costCategory_splitChargeRules :: Lens.Lens' CostCategory (Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRule))
costCategory_splitChargeRules = Lens.lens (\CostCategory' {splitChargeRules} -> splitChargeRules) (\s@CostCategory' {} a -> s {splitChargeRules = a} :: CostCategory) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
costCategory_defaultValue :: Lens.Lens' CostCategory (Prelude.Maybe Prelude.Text)
costCategory_defaultValue = Lens.lens (\CostCategory' {defaultValue} -> defaultValue) (\s@CostCategory' {} a -> s {defaultValue = a} :: CostCategory)

-- | The unique identifier for your Cost Category.
costCategory_costCategoryArn :: Lens.Lens' CostCategory Prelude.Text
costCategory_costCategoryArn = Lens.lens (\CostCategory' {costCategoryArn} -> costCategoryArn) (\s@CostCategory' {} a -> s {costCategoryArn = a} :: CostCategory)

-- | The effective state data of your Cost Category.
costCategory_effectiveStart :: Lens.Lens' CostCategory Prelude.Text
costCategory_effectiveStart = Lens.lens (\CostCategory' {effectiveStart} -> effectiveStart) (\s@CostCategory' {} a -> s {effectiveStart = a} :: CostCategory)

-- | Undocumented member.
costCategory_name :: Lens.Lens' CostCategory Prelude.Text
costCategory_name = Lens.lens (\CostCategory' {name} -> name) (\s@CostCategory' {} a -> s {name = a} :: CostCategory)

-- | Undocumented member.
costCategory_ruleVersion :: Lens.Lens' CostCategory CostCategoryRuleVersion
costCategory_ruleVersion = Lens.lens (\CostCategory' {ruleVersion} -> ruleVersion) (\s@CostCategory' {} a -> s {ruleVersion = a} :: CostCategory)

-- | The rules are processed in order. If there are multiple rules that match
-- the line item, then the first rule to match is used to determine that
-- Cost Category value.
costCategory_rules :: Lens.Lens' CostCategory (Prelude.NonEmpty CostCategoryRule)
costCategory_rules = Lens.lens (\CostCategory' {rules} -> rules) (\s@CostCategory' {} a -> s {rules = a} :: CostCategory) Prelude.. Lens.coerced

instance Core.FromJSON CostCategory where
  parseJSON =
    Core.withObject
      "CostCategory"
      ( \x ->
          CostCategory'
            Prelude.<$> ( x Core..:? "ProcessingStatus"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EffectiveEnd")
            Prelude.<*> (x Core..:? "SplitChargeRules")
            Prelude.<*> (x Core..:? "DefaultValue")
            Prelude.<*> (x Core..: "CostCategoryArn")
            Prelude.<*> (x Core..: "EffectiveStart")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "RuleVersion")
            Prelude.<*> (x Core..: "Rules")
      )

instance Prelude.Hashable CostCategory where
  hashWithSalt salt' CostCategory' {..} =
    salt' `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` ruleVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` effectiveStart
      `Prelude.hashWithSalt` costCategoryArn
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` splitChargeRules
      `Prelude.hashWithSalt` effectiveEnd
      `Prelude.hashWithSalt` processingStatus

instance Prelude.NFData CostCategory where
  rnf CostCategory' {..} =
    Prelude.rnf processingStatus
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf ruleVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf effectiveStart
      `Prelude.seq` Prelude.rnf costCategoryArn
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf splitChargeRules
      `Prelude.seq` Prelude.rnf effectiveEnd
