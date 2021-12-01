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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisionedProductPlanSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisionedProductPlanSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanType

-- | Summary information about a plan.
--
-- /See:/ 'newProvisionedProductPlanSummary' smart constructor.
data ProvisionedProductPlanSummary = ProvisionedProductPlanSummary'
  { -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the provisioned product.
    provisionProductName :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Maybe Prelude.Text,
    -- | The name of the plan.
    planName :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    planType :: Prelude.Maybe ProvisionedProductPlanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedProductPlanSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionProductId', 'provisionedProductPlanSummary_provisionProductId' - The product identifier.
--
-- 'provisioningArtifactId', 'provisionedProductPlanSummary_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'provisionProductName', 'provisionedProductPlanSummary_provisionProductName' - The user-friendly name of the provisioned product.
--
-- 'planId', 'provisionedProductPlanSummary_planId' - The plan identifier.
--
-- 'planName', 'provisionedProductPlanSummary_planName' - The name of the plan.
--
-- 'planType', 'provisionedProductPlanSummary_planType' - The plan type.
newProvisionedProductPlanSummary ::
  ProvisionedProductPlanSummary
newProvisionedProductPlanSummary =
  ProvisionedProductPlanSummary'
    { provisionProductId =
        Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      provisionProductName = Prelude.Nothing,
      planId = Prelude.Nothing,
      planName = Prelude.Nothing,
      planType = Prelude.Nothing
    }

-- | The product identifier.
provisionedProductPlanSummary_provisionProductId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisionProductId = Lens.lens (\ProvisionedProductPlanSummary' {provisionProductId} -> provisionProductId) (\s@ProvisionedProductPlanSummary' {} a -> s {provisionProductId = a} :: ProvisionedProductPlanSummary)

-- | The identifier of the provisioning artifact.
provisionedProductPlanSummary_provisioningArtifactId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisioningArtifactId = Lens.lens (\ProvisionedProductPlanSummary' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductPlanSummary' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanSummary)

-- | The user-friendly name of the provisioned product.
provisionedProductPlanSummary_provisionProductName :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisionProductName = Lens.lens (\ProvisionedProductPlanSummary' {provisionProductName} -> provisionProductName) (\s@ProvisionedProductPlanSummary' {} a -> s {provisionProductName = a} :: ProvisionedProductPlanSummary)

-- | The plan identifier.
provisionedProductPlanSummary_planId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_planId = Lens.lens (\ProvisionedProductPlanSummary' {planId} -> planId) (\s@ProvisionedProductPlanSummary' {} a -> s {planId = a} :: ProvisionedProductPlanSummary)

-- | The name of the plan.
provisionedProductPlanSummary_planName :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_planName = Lens.lens (\ProvisionedProductPlanSummary' {planName} -> planName) (\s@ProvisionedProductPlanSummary' {} a -> s {planName = a} :: ProvisionedProductPlanSummary)

-- | The plan type.
provisionedProductPlanSummary_planType :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe ProvisionedProductPlanType)
provisionedProductPlanSummary_planType = Lens.lens (\ProvisionedProductPlanSummary' {planType} -> planType) (\s@ProvisionedProductPlanSummary' {} a -> s {planType = a} :: ProvisionedProductPlanSummary)

instance Core.FromJSON ProvisionedProductPlanSummary where
  parseJSON =
    Core.withObject
      "ProvisionedProductPlanSummary"
      ( \x ->
          ProvisionedProductPlanSummary'
            Prelude.<$> (x Core..:? "ProvisionProductId")
            Prelude.<*> (x Core..:? "ProvisioningArtifactId")
            Prelude.<*> (x Core..:? "ProvisionProductName")
            Prelude.<*> (x Core..:? "PlanId")
            Prelude.<*> (x Core..:? "PlanName")
            Prelude.<*> (x Core..:? "PlanType")
      )

instance
  Prelude.Hashable
    ProvisionedProductPlanSummary
  where
  hashWithSalt salt' ProvisionedProductPlanSummary' {..} =
    salt' `Prelude.hashWithSalt` planType
      `Prelude.hashWithSalt` planName
      `Prelude.hashWithSalt` planId
      `Prelude.hashWithSalt` provisionProductName
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` provisionProductId

instance Prelude.NFData ProvisionedProductPlanSummary where
  rnf ProvisionedProductPlanSummary' {..} =
    Prelude.rnf provisionProductId
      `Prelude.seq` Prelude.rnf planType
      `Prelude.seq` Prelude.rnf planName
      `Prelude.seq` Prelude.rnf planId
      `Prelude.seq` Prelude.rnf provisionProductName
      `Prelude.seq` Prelude.rnf provisioningArtifactId
