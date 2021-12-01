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
-- Module      : Amazonka.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.PlatformProgrammingLanguage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A programming language supported by the platform.
--
-- /See:/ 'newPlatformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { -- | The name of the programming language.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the programming language.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformProgrammingLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'platformProgrammingLanguage_name' - The name of the programming language.
--
-- 'version', 'platformProgrammingLanguage_version' - The version of the programming language.
newPlatformProgrammingLanguage ::
  PlatformProgrammingLanguage
newPlatformProgrammingLanguage =
  PlatformProgrammingLanguage'
    { name =
        Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the programming language.
platformProgrammingLanguage_name :: Lens.Lens' PlatformProgrammingLanguage (Prelude.Maybe Prelude.Text)
platformProgrammingLanguage_name = Lens.lens (\PlatformProgrammingLanguage' {name} -> name) (\s@PlatformProgrammingLanguage' {} a -> s {name = a} :: PlatformProgrammingLanguage)

-- | The version of the programming language.
platformProgrammingLanguage_version :: Lens.Lens' PlatformProgrammingLanguage (Prelude.Maybe Prelude.Text)
platformProgrammingLanguage_version = Lens.lens (\PlatformProgrammingLanguage' {version} -> version) (\s@PlatformProgrammingLanguage' {} a -> s {version = a} :: PlatformProgrammingLanguage)

instance Core.FromXML PlatformProgrammingLanguage where
  parseXML x =
    PlatformProgrammingLanguage'
      Prelude.<$> (x Core..@? "Name")
      Prelude.<*> (x Core..@? "Version")

instance Prelude.Hashable PlatformProgrammingLanguage where
  hashWithSalt salt' PlatformProgrammingLanguage' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData PlatformProgrammingLanguage where
  rnf PlatformProgrammingLanguage' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version
