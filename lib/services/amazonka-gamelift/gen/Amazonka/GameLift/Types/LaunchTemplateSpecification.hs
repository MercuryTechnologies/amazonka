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
-- Module      : Amazonka.GameLift.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the GameLift FleetIQ and game server
-- groups.__
--
-- An EC2 launch template that contains configuration settings and game
-- server code to be deployed to all instances in a game server group. The
-- launch template is specified when creating a new game server group with
-- CreateGameServerGroup.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | A readable identifier for an existing EC2 launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an existing EC2 launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The version of the EC2 launch template to use. If no version is
    -- specified, the default version will be used. With Amazon EC2, you can
    -- specify a default version for a launch template. If none is set, the
    -- default is the first version created.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - A readable identifier for an existing EC2 launch template.
--
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - A unique identifier for an existing EC2 launch template.
--
-- 'version', 'launchTemplateSpecification_version' - The version of the EC2 launch template to use. If no version is
-- specified, the default version will be used. With Amazon EC2, you can
-- specify a default version for a launch template. If none is set, the
-- default is the first version created.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A readable identifier for an existing EC2 launch template.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | A unique identifier for an existing EC2 launch template.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The version of the EC2 launch template to use. If no version is
-- specified, the default version will be used. With Amazon EC2, you can
-- specify a default version for a launch template. If none is set, the
-- default is the first version created.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Prelude.Hashable LaunchTemplateSpecification where
  hashWithSalt salt' LaunchTemplateSpecification' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData LaunchTemplateSpecification where
  rnf LaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf launchTemplateId

instance Core.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LaunchTemplateName" Core..=)
              Prelude.<$> launchTemplateName,
            ("LaunchTemplateId" Core..=)
              Prelude.<$> launchTemplateId,
            ("Version" Core..=) Prelude.<$> version
          ]
      )
