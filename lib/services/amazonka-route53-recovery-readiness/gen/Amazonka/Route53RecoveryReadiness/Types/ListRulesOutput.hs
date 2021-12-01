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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.ListRulesOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.ListRulesOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of rules used in a readiness check
--
-- /See:/ 'newListRulesOutput' smart constructor.
data ListRulesOutput = ListRulesOutput'
  { -- | A description of the rule
    ruleDescription :: Prelude.Text,
    -- | The Rule\'s ID.
    ruleId :: Prelude.Text,
    -- | The resource type the rule applies to.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleDescription', 'listRulesOutput_ruleDescription' - A description of the rule
--
-- 'ruleId', 'listRulesOutput_ruleId' - The Rule\'s ID.
--
-- 'resourceType', 'listRulesOutput_resourceType' - The resource type the rule applies to.
newListRulesOutput ::
  -- | 'ruleDescription'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  ListRulesOutput
newListRulesOutput
  pRuleDescription_
  pRuleId_
  pResourceType_ =
    ListRulesOutput'
      { ruleDescription =
          pRuleDescription_,
        ruleId = pRuleId_,
        resourceType = pResourceType_
      }

-- | A description of the rule
listRulesOutput_ruleDescription :: Lens.Lens' ListRulesOutput Prelude.Text
listRulesOutput_ruleDescription = Lens.lens (\ListRulesOutput' {ruleDescription} -> ruleDescription) (\s@ListRulesOutput' {} a -> s {ruleDescription = a} :: ListRulesOutput)

-- | The Rule\'s ID.
listRulesOutput_ruleId :: Lens.Lens' ListRulesOutput Prelude.Text
listRulesOutput_ruleId = Lens.lens (\ListRulesOutput' {ruleId} -> ruleId) (\s@ListRulesOutput' {} a -> s {ruleId = a} :: ListRulesOutput)

-- | The resource type the rule applies to.
listRulesOutput_resourceType :: Lens.Lens' ListRulesOutput Prelude.Text
listRulesOutput_resourceType = Lens.lens (\ListRulesOutput' {resourceType} -> resourceType) (\s@ListRulesOutput' {} a -> s {resourceType = a} :: ListRulesOutput)

instance Core.FromJSON ListRulesOutput where
  parseJSON =
    Core.withObject
      "ListRulesOutput"
      ( \x ->
          ListRulesOutput'
            Prelude.<$> (x Core..: "ruleDescription")
            Prelude.<*> (x Core..: "ruleId")
            Prelude.<*> (x Core..: "resourceType")
      )

instance Prelude.Hashable ListRulesOutput where
  hashWithSalt salt' ListRulesOutput' {..} =
    salt' `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleDescription

instance Prelude.NFData ListRulesOutput where
  rnf ListRulesOutput' {..} =
    Prelude.rnf ruleDescription
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf ruleId
