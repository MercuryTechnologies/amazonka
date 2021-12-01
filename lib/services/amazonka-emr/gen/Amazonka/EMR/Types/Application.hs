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
-- Module      : Amazonka.EMR.Types.Application
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | With Amazon EMR release version 4.0 and later, the only accepted
-- parameter is the application name. To pass arguments to applications,
-- you use configuration classifications specified using configuration JSON
-- objects. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications>.
--
-- With earlier Amazon EMR releases, the application is any Amazon or
-- third-party software that you can add to the cluster. This structure
-- contains a list of strings that indicates the software to use with the
-- cluster and accepts a user argument list. Amazon EMR accepts and
-- forwards the argument list to the corresponding installation script as
-- bootstrap action argument.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | Arguments for Amazon EMR to pass to the application.
    args :: Prelude.Maybe [Prelude.Text],
    -- | This option is for advanced users only. This is meta information about
    -- third-party applications that third-party vendors use for testing
    -- purposes.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the application.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'application_args' - Arguments for Amazon EMR to pass to the application.
--
-- 'additionalInfo', 'application_additionalInfo' - This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
--
-- 'name', 'application_name' - The name of the application.
--
-- 'version', 'application_version' - The version of the application.
newApplication ::
  Application
newApplication =
  Application'
    { args = Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Arguments for Amazon EMR to pass to the application.
application_args :: Lens.Lens' Application (Prelude.Maybe [Prelude.Text])
application_args = Lens.lens (\Application' {args} -> args) (\s@Application' {} a -> s {args = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
application_additionalInfo :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_additionalInfo = Lens.lens (\Application' {additionalInfo} -> additionalInfo) (\s@Application' {} a -> s {additionalInfo = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The version of the application.
application_version :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_version = Lens.lens (\Application' {version} -> version) (\s@Application' {} a -> s {version = a} :: Application)

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Core..:? "Args" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AdditionalInfo" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable Application where
  hashWithSalt salt' Application' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` args

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf args `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf additionalInfo

instance Core.ToJSON Application where
  toJSON Application' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Args" Core..=) Prelude.<$> args,
            ("AdditionalInfo" Core..=)
              Prelude.<$> additionalInfo,
            ("Name" Core..=) Prelude.<$> name,
            ("Version" Core..=) Prelude.<$> version
          ]
      )
