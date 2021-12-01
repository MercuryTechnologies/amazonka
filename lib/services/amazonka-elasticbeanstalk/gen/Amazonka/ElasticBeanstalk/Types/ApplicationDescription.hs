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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationDescription where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of an application.
--
-- /See:/ 'newApplicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The names of the versions for this application.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | The date when the application was last modified.
    dateUpdated :: Prelude.Maybe Core.ISO8601,
    -- | The date when the application was created.
    dateCreated :: Prelude.Maybe Core.ISO8601,
    -- | The name of the application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The names of the configuration templates associated with this
    -- application.
    configurationTemplates :: Prelude.Maybe [Prelude.Text],
    -- | The lifecycle settings for the application.
    resourceLifecycleConfig :: Prelude.Maybe ApplicationResourceLifecycleConfig,
    -- | User-defined description of the application.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'applicationDescription_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'versions', 'applicationDescription_versions' - The names of the versions for this application.
--
-- 'dateUpdated', 'applicationDescription_dateUpdated' - The date when the application was last modified.
--
-- 'dateCreated', 'applicationDescription_dateCreated' - The date when the application was created.
--
-- 'applicationName', 'applicationDescription_applicationName' - The name of the application.
--
-- 'configurationTemplates', 'applicationDescription_configurationTemplates' - The names of the configuration templates associated with this
-- application.
--
-- 'resourceLifecycleConfig', 'applicationDescription_resourceLifecycleConfig' - The lifecycle settings for the application.
--
-- 'description', 'applicationDescription_description' - User-defined description of the application.
newApplicationDescription ::
  ApplicationDescription
newApplicationDescription =
  ApplicationDescription'
    { applicationArn =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      configurationTemplates = Prelude.Nothing,
      resourceLifecycleConfig = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
applicationDescription_applicationArn :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_applicationArn = Lens.lens (\ApplicationDescription' {applicationArn} -> applicationArn) (\s@ApplicationDescription' {} a -> s {applicationArn = a} :: ApplicationDescription)

-- | The names of the versions for this application.
applicationDescription_versions :: Lens.Lens' ApplicationDescription (Prelude.Maybe [Prelude.Text])
applicationDescription_versions = Lens.lens (\ApplicationDescription' {versions} -> versions) (\s@ApplicationDescription' {} a -> s {versions = a} :: ApplicationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date when the application was last modified.
applicationDescription_dateUpdated :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.UTCTime)
applicationDescription_dateUpdated = Lens.lens (\ApplicationDescription' {dateUpdated} -> dateUpdated) (\s@ApplicationDescription' {} a -> s {dateUpdated = a} :: ApplicationDescription) Prelude.. Lens.mapping Core._Time

-- | The date when the application was created.
applicationDescription_dateCreated :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.UTCTime)
applicationDescription_dateCreated = Lens.lens (\ApplicationDescription' {dateCreated} -> dateCreated) (\s@ApplicationDescription' {} a -> s {dateCreated = a} :: ApplicationDescription) Prelude.. Lens.mapping Core._Time

-- | The name of the application.
applicationDescription_applicationName :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_applicationName = Lens.lens (\ApplicationDescription' {applicationName} -> applicationName) (\s@ApplicationDescription' {} a -> s {applicationName = a} :: ApplicationDescription)

-- | The names of the configuration templates associated with this
-- application.
applicationDescription_configurationTemplates :: Lens.Lens' ApplicationDescription (Prelude.Maybe [Prelude.Text])
applicationDescription_configurationTemplates = Lens.lens (\ApplicationDescription' {configurationTemplates} -> configurationTemplates) (\s@ApplicationDescription' {} a -> s {configurationTemplates = a} :: ApplicationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The lifecycle settings for the application.
applicationDescription_resourceLifecycleConfig :: Lens.Lens' ApplicationDescription (Prelude.Maybe ApplicationResourceLifecycleConfig)
applicationDescription_resourceLifecycleConfig = Lens.lens (\ApplicationDescription' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@ApplicationDescription' {} a -> s {resourceLifecycleConfig = a} :: ApplicationDescription)

-- | User-defined description of the application.
applicationDescription_description :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_description = Lens.lens (\ApplicationDescription' {description} -> description) (\s@ApplicationDescription' {} a -> s {description = a} :: ApplicationDescription)

instance Core.FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      Prelude.<$> (x Core..@? "ApplicationArn")
      Prelude.<*> ( x Core..@? "Versions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "DateUpdated")
      Prelude.<*> (x Core..@? "DateCreated")
      Prelude.<*> (x Core..@? "ApplicationName")
      Prelude.<*> ( x Core..@? "ConfigurationTemplates"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ResourceLifecycleConfig")
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable ApplicationDescription where
  hashWithSalt salt' ApplicationDescription' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` resourceLifecycleConfig
      `Prelude.hashWithSalt` configurationTemplates
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` versions
      `Prelude.hashWithSalt` applicationArn

instance Prelude.NFData ApplicationDescription where
  rnf ApplicationDescription' {..} =
    Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf resourceLifecycleConfig
      `Prelude.seq` Prelude.rnf configurationTemplates
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf versions
