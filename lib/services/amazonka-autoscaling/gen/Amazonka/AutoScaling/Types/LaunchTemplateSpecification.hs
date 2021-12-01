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
-- Module      : Amazonka.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the launch template and the version of the launch template
-- that Amazon EC2 Auto Scaling uses to launch Amazon EC2 instances. For
-- more information about launch templates, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/LaunchTemplates.html Launch templates>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | The name of the launch template. To get the template name, use the
    -- Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
    -- API operation. New launch templates can be created using the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
    -- API.
    --
    -- Conditional: You must specify either a @LaunchTemplateId@ or a
    -- @LaunchTemplateName@.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template. To get the template ID, use the Amazon
    -- EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
    -- API operation. New launch templates can be created using the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
    -- API.
    --
    -- Conditional: You must specify either a @LaunchTemplateId@ or a
    -- @LaunchTemplateName@.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The version number, @$Latest@, or @$Default@. To get the version number,
    -- use the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
    -- API operation. New launch template versions can be created using the
    -- Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
    -- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
    -- latest version of the launch template when launching instances. If the
    -- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
    -- of the launch template when launching instances. The default value is
    -- @$Default@.
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
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template. To get the template name, use the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
--
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template. To get the template ID, use the Amazon
-- EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
--
-- 'version', 'launchTemplateSpecification_version' - The version number, @$Latest@, or @$Default@. To get the version number,
-- use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
-- API operation. New launch template versions can be created using the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
-- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
-- latest version of the launch template when launching instances. If the
-- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
-- of the launch template when launching instances. The default value is
-- @$Default@.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the launch template. To get the template name, use the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The ID of the launch template. To get the template ID, use the Amazon
-- EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The version number, @$Latest@, or @$Default@. To get the version number,
-- use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
-- API operation. New launch template versions can be created using the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
-- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
-- latest version of the launch template when launching instances. If the
-- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
-- of the launch template when launching instances. The default value is
-- @$Default@.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Core.FromXML LaunchTemplateSpecification where
  parseXML x =
    LaunchTemplateSpecification'
      Prelude.<$> (x Core..@? "LaunchTemplateName")
      Prelude.<*> (x Core..@? "LaunchTemplateId")
      Prelude.<*> (x Core..@? "Version")

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

instance Core.ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    Prelude.mconcat
      [ "LaunchTemplateName" Core.=: launchTemplateName,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "Version" Core.=: version
      ]
