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
-- Module      : Amazonka.IoTFleetHub.Types.ApplicationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetHub.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTFleetHub.Types.ApplicationState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a AWS IoT Device Management web
-- application.
--
-- Fleet Hub for AWS IoT Device Management is in public preview and is
-- subject to change.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | An optional description of the web application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | The current state of the web application.
    applicationState :: Prelude.Maybe ApplicationState,
    -- | The date (in Unix epoch time) when the web application was created.
    applicationCreationDate :: Prelude.Maybe Prelude.Integer,
    -- | The date (in Unix epoch time) when the web application was last updated.
    applicationLastUpdateDate :: Prelude.Maybe Prelude.Integer,
    -- | The unique Id of the web application.
    applicationId :: Prelude.Text,
    -- | The name of the web application.
    applicationName :: Prelude.Text,
    -- | The URL of the web application.
    applicationUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationDescription', 'applicationSummary_applicationDescription' - An optional description of the web application.
--
-- 'applicationState', 'applicationSummary_applicationState' - The current state of the web application.
--
-- 'applicationCreationDate', 'applicationSummary_applicationCreationDate' - The date (in Unix epoch time) when the web application was created.
--
-- 'applicationLastUpdateDate', 'applicationSummary_applicationLastUpdateDate' - The date (in Unix epoch time) when the web application was last updated.
--
-- 'applicationId', 'applicationSummary_applicationId' - The unique Id of the web application.
--
-- 'applicationName', 'applicationSummary_applicationName' - The name of the web application.
--
-- 'applicationUrl', 'applicationSummary_applicationUrl' - The URL of the web application.
newApplicationSummary ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'applicationUrl'
  Prelude.Text ->
  ApplicationSummary
newApplicationSummary
  pApplicationId_
  pApplicationName_
  pApplicationUrl_ =
    ApplicationSummary'
      { applicationDescription =
          Prelude.Nothing,
        applicationState = Prelude.Nothing,
        applicationCreationDate = Prelude.Nothing,
        applicationLastUpdateDate = Prelude.Nothing,
        applicationId = pApplicationId_,
        applicationName = pApplicationName_,
        applicationUrl = pApplicationUrl_
      }

-- | An optional description of the web application.
applicationSummary_applicationDescription :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_applicationDescription = Lens.lens (\ApplicationSummary' {applicationDescription} -> applicationDescription) (\s@ApplicationSummary' {} a -> s {applicationDescription = a} :: ApplicationSummary)

-- | The current state of the web application.
applicationSummary_applicationState :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApplicationState)
applicationSummary_applicationState = Lens.lens (\ApplicationSummary' {applicationState} -> applicationState) (\s@ApplicationSummary' {} a -> s {applicationState = a} :: ApplicationSummary)

-- | The date (in Unix epoch time) when the web application was created.
applicationSummary_applicationCreationDate :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Integer)
applicationSummary_applicationCreationDate = Lens.lens (\ApplicationSummary' {applicationCreationDate} -> applicationCreationDate) (\s@ApplicationSummary' {} a -> s {applicationCreationDate = a} :: ApplicationSummary)

-- | The date (in Unix epoch time) when the web application was last updated.
applicationSummary_applicationLastUpdateDate :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Integer)
applicationSummary_applicationLastUpdateDate = Lens.lens (\ApplicationSummary' {applicationLastUpdateDate} -> applicationLastUpdateDate) (\s@ApplicationSummary' {} a -> s {applicationLastUpdateDate = a} :: ApplicationSummary)

-- | The unique Id of the web application.
applicationSummary_applicationId :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationId = Lens.lens (\ApplicationSummary' {applicationId} -> applicationId) (\s@ApplicationSummary' {} a -> s {applicationId = a} :: ApplicationSummary)

-- | The name of the web application.
applicationSummary_applicationName :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationName = Lens.lens (\ApplicationSummary' {applicationName} -> applicationName) (\s@ApplicationSummary' {} a -> s {applicationName = a} :: ApplicationSummary)

-- | The URL of the web application.
applicationSummary_applicationUrl :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationUrl = Lens.lens (\ApplicationSummary' {applicationUrl} -> applicationUrl) (\s@ApplicationSummary' {} a -> s {applicationUrl = a} :: ApplicationSummary)

instance Core.FromJSON ApplicationSummary where
  parseJSON =
    Core.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Core..:? "applicationDescription")
            Prelude.<*> (x Core..:? "applicationState")
            Prelude.<*> (x Core..:? "applicationCreationDate")
            Prelude.<*> (x Core..:? "applicationLastUpdateDate")
            Prelude.<*> (x Core..: "applicationId")
            Prelude.<*> (x Core..: "applicationName")
            Prelude.<*> (x Core..: "applicationUrl")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt salt' ApplicationSummary' {..} =
    salt' `Prelude.hashWithSalt` applicationUrl
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationLastUpdateDate
      `Prelude.hashWithSalt` applicationCreationDate
      `Prelude.hashWithSalt` applicationState
      `Prelude.hashWithSalt` applicationDescription

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf applicationDescription
      `Prelude.seq` Prelude.rnf applicationUrl
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationLastUpdateDate
      `Prelude.seq` Prelude.rnf applicationCreationDate
      `Prelude.seq` Prelude.rnf applicationState
