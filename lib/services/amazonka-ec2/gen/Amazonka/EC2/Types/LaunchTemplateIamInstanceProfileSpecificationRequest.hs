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
-- Module      : Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An IAM instance profile.
--
-- /See:/ 'newLaunchTemplateIamInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIamInstanceProfileSpecificationRequest = LaunchTemplateIamInstanceProfileSpecificationRequest'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateIamInstanceProfileSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'launchTemplateIamInstanceProfileSpecificationRequest_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'launchTemplateIamInstanceProfileSpecificationRequest_name' - The name of the instance profile.
newLaunchTemplateIamInstanceProfileSpecificationRequest ::
  LaunchTemplateIamInstanceProfileSpecificationRequest
newLaunchTemplateIamInstanceProfileSpecificationRequest =
  LaunchTemplateIamInstanceProfileSpecificationRequest'
    { arn =
        Prelude.Nothing,
      name =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
launchTemplateIamInstanceProfileSpecificationRequest_arn :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateIamInstanceProfileSpecificationRequest_arn = Lens.lens (\LaunchTemplateIamInstanceProfileSpecificationRequest' {arn} -> arn) (\s@LaunchTemplateIamInstanceProfileSpecificationRequest' {} a -> s {arn = a} :: LaunchTemplateIamInstanceProfileSpecificationRequest)

-- | The name of the instance profile.
launchTemplateIamInstanceProfileSpecificationRequest_name :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateIamInstanceProfileSpecificationRequest_name = Lens.lens (\LaunchTemplateIamInstanceProfileSpecificationRequest' {name} -> name) (\s@LaunchTemplateIamInstanceProfileSpecificationRequest' {} a -> s {name = a} :: LaunchTemplateIamInstanceProfileSpecificationRequest)

instance
  Prelude.Hashable
    LaunchTemplateIamInstanceProfileSpecificationRequest
  where
  hashWithSalt
    salt'
    LaunchTemplateIamInstanceProfileSpecificationRequest' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    LaunchTemplateIamInstanceProfileSpecificationRequest
  where
  rnf
    LaunchTemplateIamInstanceProfileSpecificationRequest' {..} =
      Prelude.rnf arn `Prelude.seq` Prelude.rnf name

instance
  Core.ToQuery
    LaunchTemplateIamInstanceProfileSpecificationRequest
  where
  toQuery
    LaunchTemplateIamInstanceProfileSpecificationRequest' {..} =
      Prelude.mconcat
        ["Arn" Core.=: arn, "Name" Core.=: name]
