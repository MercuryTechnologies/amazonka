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
-- Module      : Amazonka.SSMIncidents.Types.IncidentRecordSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.IncidentRecordSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about how the incident record was created and when.
--
-- /See:/ 'newIncidentRecordSource' smart constructor.
data IncidentRecordSource = IncidentRecordSource'
  { -- | The principal the assumed the role specified of the @createdBy@.
    invokedBy :: Prelude.Maybe Prelude.Text,
    -- | The resource that caused the incident to be created.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The principal that started the incident.
    createdBy :: Prelude.Text,
    -- | The service that started the incident. This can be manually created from
    -- Incident Manager, automatically created using an AWS CloudWatch alarm,
    -- or Amazon EventBridge event.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncidentRecordSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invokedBy', 'incidentRecordSource_invokedBy' - The principal the assumed the role specified of the @createdBy@.
--
-- 'resourceArn', 'incidentRecordSource_resourceArn' - The resource that caused the incident to be created.
--
-- 'createdBy', 'incidentRecordSource_createdBy' - The principal that started the incident.
--
-- 'source', 'incidentRecordSource_source' - The service that started the incident. This can be manually created from
-- Incident Manager, automatically created using an AWS CloudWatch alarm,
-- or Amazon EventBridge event.
newIncidentRecordSource ::
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  IncidentRecordSource
newIncidentRecordSource pCreatedBy_ pSource_ =
  IncidentRecordSource'
    { invokedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      createdBy = pCreatedBy_,
      source = pSource_
    }

-- | The principal the assumed the role specified of the @createdBy@.
incidentRecordSource_invokedBy :: Lens.Lens' IncidentRecordSource (Prelude.Maybe Prelude.Text)
incidentRecordSource_invokedBy = Lens.lens (\IncidentRecordSource' {invokedBy} -> invokedBy) (\s@IncidentRecordSource' {} a -> s {invokedBy = a} :: IncidentRecordSource)

-- | The resource that caused the incident to be created.
incidentRecordSource_resourceArn :: Lens.Lens' IncidentRecordSource (Prelude.Maybe Prelude.Text)
incidentRecordSource_resourceArn = Lens.lens (\IncidentRecordSource' {resourceArn} -> resourceArn) (\s@IncidentRecordSource' {} a -> s {resourceArn = a} :: IncidentRecordSource)

-- | The principal that started the incident.
incidentRecordSource_createdBy :: Lens.Lens' IncidentRecordSource Prelude.Text
incidentRecordSource_createdBy = Lens.lens (\IncidentRecordSource' {createdBy} -> createdBy) (\s@IncidentRecordSource' {} a -> s {createdBy = a} :: IncidentRecordSource)

-- | The service that started the incident. This can be manually created from
-- Incident Manager, automatically created using an AWS CloudWatch alarm,
-- or Amazon EventBridge event.
incidentRecordSource_source :: Lens.Lens' IncidentRecordSource Prelude.Text
incidentRecordSource_source = Lens.lens (\IncidentRecordSource' {source} -> source) (\s@IncidentRecordSource' {} a -> s {source = a} :: IncidentRecordSource)

instance Core.FromJSON IncidentRecordSource where
  parseJSON =
    Core.withObject
      "IncidentRecordSource"
      ( \x ->
          IncidentRecordSource'
            Prelude.<$> (x Core..:? "invokedBy")
            Prelude.<*> (x Core..:? "resourceArn")
            Prelude.<*> (x Core..: "createdBy")
            Prelude.<*> (x Core..: "source")
      )

instance Prelude.Hashable IncidentRecordSource where
  hashWithSalt salt' IncidentRecordSource' {..} =
    salt' `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` invokedBy

instance Prelude.NFData IncidentRecordSource where
  rnf IncidentRecordSource' {..} =
    Prelude.rnf invokedBy
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf resourceArn
