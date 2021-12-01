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
-- Module      : Amazonka.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InsightImpactGraphService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.InsightImpactGraphEdge

-- | Information about an application that processed requests, users that
-- made requests, or downstream services, resources, and applications that
-- an application used.
--
-- /See:/ 'newInsightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { -- | Identifier for the service. Unique within the service map.
    referenceId :: Prelude.Maybe Prelude.Int,
    -- | Identifier of the Amazon Web Services account in which the service runs.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | A list of names for the service, including the canonical name.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The canonical name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the service. Unique within the service map.
    --
    -- -   Amazon Web Services Resource - The type of an Amazon Web Services
    --     resource. For example, AWS::EC2::Instance for an application running
    --     on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table
    --     that the application used.
    --
    -- -   Amazon Web Services Service - The type of an Amazon Web Services
    --     service. For example, AWS::DynamoDB for downstream calls to Amazon
    --     DynamoDB that didn\'t target a specific table.
    --
    -- -   Amazon Web Services Service - The type of an Amazon Web Services
    --     service. For example, AWS::DynamoDB for downstream calls to Amazon
    --     DynamoDB that didn\'t target a specific table.
    --
    -- -   remote - A downstream service of indeterminate type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | Connections to downstream services.
    edges :: Prelude.Maybe [InsightImpactGraphEdge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightImpactGraphService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceId', 'insightImpactGraphService_referenceId' - Identifier for the service. Unique within the service map.
--
-- 'accountId', 'insightImpactGraphService_accountId' - Identifier of the Amazon Web Services account in which the service runs.
--
-- 'names', 'insightImpactGraphService_names' - A list of names for the service, including the canonical name.
--
-- 'name', 'insightImpactGraphService_name' - The canonical name of the service.
--
-- 'type'', 'insightImpactGraphService_type' - Identifier for the service. Unique within the service map.
--
-- -   Amazon Web Services Resource - The type of an Amazon Web Services
--     resource. For example, AWS::EC2::Instance for an application running
--     on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table
--     that the application used.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, AWS::DynamoDB for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, AWS::DynamoDB for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   remote - A downstream service of indeterminate type.
--
-- 'edges', 'insightImpactGraphService_edges' - Connections to downstream services.
newInsightImpactGraphService ::
  InsightImpactGraphService
newInsightImpactGraphService =
  InsightImpactGraphService'
    { referenceId =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      names = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      edges = Prelude.Nothing
    }

-- | Identifier for the service. Unique within the service map.
insightImpactGraphService_referenceId :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Int)
insightImpactGraphService_referenceId = Lens.lens (\InsightImpactGraphService' {referenceId} -> referenceId) (\s@InsightImpactGraphService' {} a -> s {referenceId = a} :: InsightImpactGraphService)

-- | Identifier of the Amazon Web Services account in which the service runs.
insightImpactGraphService_accountId :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
insightImpactGraphService_accountId = Lens.lens (\InsightImpactGraphService' {accountId} -> accountId) (\s@InsightImpactGraphService' {} a -> s {accountId = a} :: InsightImpactGraphService)

-- | A list of names for the service, including the canonical name.
insightImpactGraphService_names :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe [Prelude.Text])
insightImpactGraphService_names = Lens.lens (\InsightImpactGraphService' {names} -> names) (\s@InsightImpactGraphService' {} a -> s {names = a} :: InsightImpactGraphService) Prelude.. Lens.mapping Lens.coerced

-- | The canonical name of the service.
insightImpactGraphService_name :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
insightImpactGraphService_name = Lens.lens (\InsightImpactGraphService' {name} -> name) (\s@InsightImpactGraphService' {} a -> s {name = a} :: InsightImpactGraphService)

-- | Identifier for the service. Unique within the service map.
--
-- -   Amazon Web Services Resource - The type of an Amazon Web Services
--     resource. For example, AWS::EC2::Instance for an application running
--     on Amazon EC2 or AWS::DynamoDB::Table for an Amazon DynamoDB table
--     that the application used.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, AWS::DynamoDB for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   Amazon Web Services Service - The type of an Amazon Web Services
--     service. For example, AWS::DynamoDB for downstream calls to Amazon
--     DynamoDB that didn\'t target a specific table.
--
-- -   remote - A downstream service of indeterminate type.
insightImpactGraphService_type :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
insightImpactGraphService_type = Lens.lens (\InsightImpactGraphService' {type'} -> type') (\s@InsightImpactGraphService' {} a -> s {type' = a} :: InsightImpactGraphService)

-- | Connections to downstream services.
insightImpactGraphService_edges :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe [InsightImpactGraphEdge])
insightImpactGraphService_edges = Lens.lens (\InsightImpactGraphService' {edges} -> edges) (\s@InsightImpactGraphService' {} a -> s {edges = a} :: InsightImpactGraphService) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON InsightImpactGraphService where
  parseJSON =
    Core.withObject
      "InsightImpactGraphService"
      ( \x ->
          InsightImpactGraphService'
            Prelude.<$> (x Core..:? "ReferenceId")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Edges" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable InsightImpactGraphService where
  hashWithSalt salt' InsightImpactGraphService' {..} =
    salt' `Prelude.hashWithSalt` edges
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` referenceId

instance Prelude.NFData InsightImpactGraphService where
  rnf InsightImpactGraphService' {..} =
    Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf edges
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf accountId
